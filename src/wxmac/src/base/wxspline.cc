/*
 * File:      wxspline.cc
 * Purpose:     
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wxspline.cc,v 1.2 1994/08/14 21:34:01 edz Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

// Has to be a comment as this file is #include\'d from wb_dc.cc
/* static const char sccsid[] = "@(#)wxspline.cc        1.3 5/9/94"; */

/*
 * Artificial Intelligence Applications Institute,                     
 * The University of Edinburgh
 */


/*
 *  Cubic B spline drawing code based on:
 *  Computer Graphics - A Programming Approach
 *  Steven Harrington (2nd edition)
 *  Published by McGraw-Hill
 *  Chapter 11 p400 - 417

 *  A B-spline is an interpolation curve that does not necessarily pass
 *  through all the sample points. This code implements a cubic B-spline
 *  function. The cubic B-spline functions interpolate over 4 sample
 *  points.

 *  The mathematical function that defines the curve can be expressed in
 *  parametric form as:
 * 
 *            X = fX(u) = sum(i to n) XiBi(u)
 *            Y = fY(u) = sum(i to n) YiBi(u)
 *
 *  where Bi(u) is called a blending function.
 *
 *  The function DrawSpline defines the blending functions for 
 *  a cubic B-spline, which are cubic polynomials in u. Special blending
 *  functions are required for the first, second, next to last and last
 *  section of the curve (a section is defined by 2 of the sample points),
 *  as well as the standard blending function. These special blending 
 *  functions are needed because of the requirement that the spline 
 *  must pass through the first and last sample points.
 *
 *  DrawSpline works by calculating points on the spline
 *  (at various values of u) and then drawing straight lines
 *  between the points to approximate the spline. Points on the spline 
 *  are calculated for each section of the curve, moving along one 
 *  section at a time until we reach the end of the sample points.
 */

static wxPoint *startPoint = NULL;
static wxPoint *samplePoints = NULL;

// function prototype
static void allocBlendArrays (double **mainBlend[],
			      double **firstBlend[],
			      double **secondBlend[],
			      double **penultimateBlend[],
			      double **lastBlend[],
			      int nSegments,
			      int numberOfLines);

// function prototype
static void initBSpline (int numberOfLines,
			 double *mainBlend[],
			 double *firstBlend[],
			 double *secondBlend[],
			 double *penultimateBlend[],
			 double *lastBlend[]);

// function prototype
static void MakeCurve (wxbDC * dc, wxPoint samplePoints[],
		       int numberOfLines,
		       double *blendFunction[],
		       wxPoint startPoint[]);

// function prototype
static void nextSection (wxPoint samplePoints[]);

// function prototype
static void nextSamplePoint (int &currentPoint,
			     wxPoint samplePoints[],
			     wxPoint points[]);

/*************************************************************************
 * This is the main function that draws the spline for a given set of
 * sample points. It's arguments are:
 *
 *   1) dc -- the device on which to draw the spline,
 *   2) nPoints -- the number of sample points
 *   3) an array of structures, each structure containing a x and y coordinate
 *      of a sample point. The array is of size nPoints -- this is
 *      guaranteed by the calling function -- so no checking is done.
 */

void wxbDC::DrawSpline (wxList * list)
{
  wxPoint *points = new wxPoint[list->Number ()];
  int i = 0;

  for(wxNode *node = list->First (); node; node = node->Next())
    {
      wxPoint *point = (wxPoint *) node->Data ();
      points[i].x = point->x;
      points[i++].y = point->y;
    }
  DrawSpline (i, points);
  delete[]points;
}

void wxbDC::DrawSpline (int nPoints, wxPoint points[])
{
/*
   First declare the number of line segments per section, to make
   it easy to change this value if more line segments are required
   to better draw the spline. When numberOfLines = 3, you can see that
   the spline is made up of straight line segments, but when 
   numberOfLines = 30, the spline looks smooth.
 */

  const int numberOfLines = 30;

/* declare the array to hold the x and y coordinates for the start of
   each spline segment that is drawn. startPoint[0].x and startPoint[0].y
   are constantly updated, after the spline segment is drawn, to be the 
   end of the last drawn spline segment -- see function MakeCurve.
 */

  startPoint = new wxPoint[1];

/* declare the array to hold the sample points that are used to calculate
   the x and y coordinates for the spline segments. To draw a spline between
   any 2 sample points actually requires taking into account 4 sample points
 */

  samplePoints = new wxPoint[4];

// declare the arrays to hold the blending functions

  double **mainBlend = NULL;
  double **firstBlend = NULL;
  double **secondBlend = NULL;
  double **penultimateBlend = NULL;
  double **lastBlend = NULL;

// declare the second dimension of the array, which is due to the number
  // of sample points taken into account when drawing a spline

  const int nSegments = 4;

// Allocate memory for the blending function arrays

  allocBlendArrays (&mainBlend, &firstBlend, &secondBlend,
		    &penultimateBlend, &lastBlend,
		    nSegments, numberOfLines);

// instantiate the blending function arrays with the appropriate values

  initBSpline (numberOfLines, mainBlend, firstBlend,
	       secondBlend, penultimateBlend, lastBlend);

/* 0, 1 and 2 sample points are special cases and are handled separately */

  int currentPoint = 0;

  if (nPoints <= 1)
    {
      return;
    }
  if (nPoints == 2)
    {
      startPoint[0].x = points[0].x;
      startPoint[0].y = points[0].y;
      float x = points[1].x;
      float y = points[1].y;
      DrawLine (startPoint[0].x, startPoint[0].y, x, y);
    }

/* 3 - 7 sample points are handled by repeating the first and last point
 * an equal number of times, such that the number of sample points becomes 
 * >= 8. The reason for this is that the algorithm really expects a minimum
 * of 8 sample points.
 * 
 * Hence the array samplePoints will contain repetitions of points in the
 * array called points (which are the actual sample points that the function
 * is called with.
 */

  if (nPoints == 3)
    {
      currentPoint = 0;
      startPoint[0].x = points[0].x;
      startPoint[0].y = points[0].y;
      for (int i = 0; i < 4; i++)
	{
	  samplePoints[i].x = points[0].x;
	  samplePoints[i].y = points[0].y;
	}
      MakeCurve (this, samplePoints, numberOfLines, firstBlend, startPoint);
      nextSection (samplePoints);
      nextSamplePoint (currentPoint, samplePoints, points);
      MakeCurve (this, samplePoints, numberOfLines,
		 secondBlend, startPoint);
      nextSection (samplePoints);
      nextSamplePoint (currentPoint, samplePoints, points);
      MakeCurve (this, samplePoints, numberOfLines,
		 mainBlend, startPoint);
      nextSection (samplePoints);
// reached the last sample point, so compensate for the fact that
      // currentPoint will be incremented by 1 in the function nextSamplePoint
      currentPoint = currentPoint - 1;
      nextSamplePoint (currentPoint, samplePoints, points);
      MakeCurve (this, samplePoints, numberOfLines,
		 mainBlend, startPoint);
      nextSection (samplePoints);
      currentPoint = currentPoint - 1;
      nextSamplePoint (currentPoint, samplePoints, points);
      MakeCurve (this, samplePoints, numberOfLines,
		 penultimateBlend, startPoint);
      nextSection (samplePoints);
      currentPoint = currentPoint - 1;
      nextSamplePoint (currentPoint, samplePoints, points);
      MakeCurve (this, samplePoints, numberOfLines,
		 lastBlend, startPoint);
    }

  if (nPoints >= 4 && nPoints < 8)
    {
      currentPoint = 0;
      startPoint[0].x = points[0].x;
      startPoint[0].y = points[0].y;
      for (int i = 0; i < 3; i++)
	{
	  samplePoints[i].x = points[0].x;
	  samplePoints[i].y = points[0].y;
	}
      samplePoints[3].x = points[1].x;
      samplePoints[3].y = points[1].y;
      currentPoint = currentPoint + 1;
      MakeCurve (this, samplePoints, numberOfLines, firstBlend, startPoint);
      nextSection (samplePoints);
      nextSamplePoint (currentPoint, samplePoints, points);
      MakeCurve (this, samplePoints, numberOfLines,
		 secondBlend, startPoint);
      nextSection (samplePoints);
      for (int j = 0; j < (nPoints - 3); j++)
	{
	  nextSamplePoint (currentPoint, samplePoints, points);
	  MakeCurve (this, samplePoints, numberOfLines,
		     mainBlend, startPoint);
	  nextSection (samplePoints);
	}
// reached the last sample point, so compensate for the fact that
      // currentPoint will be incremented by 1 in the function nextSamplePoint
      currentPoint = currentPoint - 1;
      nextSamplePoint (currentPoint, samplePoints, points);
      MakeCurve (this, samplePoints, numberOfLines,
		 penultimateBlend, startPoint);
      nextSection (samplePoints);
      currentPoint = currentPoint - 1;
      nextSamplePoint (currentPoint, samplePoints, points);
      MakeCurve (this, samplePoints, numberOfLines,
		 lastBlend, startPoint);
    }
  if (nPoints >= 8)
    {
      currentPoint = 0;
      startPoint[0].x = points[0].x;
      startPoint[0].y = points[0].y;
      for (int i = 0; i < 4; i++)
	{
	  samplePoints[i].x = points[i].x;
	  samplePoints[i].y = points[i].y;
	}
      currentPoint = currentPoint + 3;
      MakeCurve (this, samplePoints, numberOfLines, firstBlend, startPoint);
      nextSection (samplePoints);
      nextSamplePoint (currentPoint, samplePoints, points);
      MakeCurve (this, samplePoints, numberOfLines,
		 secondBlend, startPoint);
      nextSection (samplePoints);
      while (currentPoint < (nPoints - 3))
	{
	  nextSamplePoint (currentPoint, samplePoints, points);
	  MakeCurve (this, samplePoints, numberOfLines,
		     mainBlend, startPoint);
	  nextSection (samplePoints);
	}
      nextSamplePoint (currentPoint, samplePoints, points);
      MakeCurve (this, samplePoints, numberOfLines,
		 penultimateBlend, startPoint);
      nextSection (samplePoints);
      nextSamplePoint (currentPoint, samplePoints, points);
      MakeCurve (this, samplePoints, numberOfLines,
		 lastBlend, startPoint);
    }
}



/*************************************************************************
 * This function alocates memory for a blending function array
 */

static void 
allocArray (double ***ar, int nSegments, int numberOfLines)
{
  *ar = new double *[nSegments];
  for (int i = 0; i < nSegments; i++)
    (*ar)[i] = new double[numberOfLines];
}

/*************************************************************************
 * This function alocates memory for all the blending function arrays
 */

static void 
allocBlendArrays (double **mainBlend[],
		  double **firstBlend[],
		  double **secondBlend[],
		  double **penultimateBlend[],
		  double **lastBlend[],
		  int nSegments,
		  int numberOfLines)
{
  allocArray (mainBlend, nSegments, numberOfLines);
  allocArray (firstBlend, nSegments, numberOfLines);
  allocArray (secondBlend, nSegments, numberOfLines);
  allocArray (penultimateBlend, nSegments, numberOfLines);
  allocArray (lastBlend, nSegments, numberOfLines);
}

/*************************************************************************
 * This function initialise the spline blending function arrays.
 * It's arguments are:
 *
 * 1) numberOfLines -- the number of segment lines to use to draw
 *                     the spline between two sample points.
 *	               This is a set at the top of the file.
 * 2) - 6) Blending function arrays of size 4 by numberOfLines.
 *         The reason the first array dimension is 4 is that 4 sample
 *         points are required by this algorithm to calculate
 *         the x and y coordinates of points on the spline to be drawn.
 *	   
 *         The values in these arrays are constant, irrespective of
 *	   the sample points.
 */

static void 
initBSpline (int numberOfLines,
	     double *mainBlend[],
	     double *firstBlend[],
	     double *secondBlend[],
	     double *penultimateBlend[],
	     double *lastBlend[])

/*
 * The cubic B-spline blending function interpolates over 4 sample points
 * Thus in the code below the first index of the the Blending function array,
 * is either 0,1,2,3. The second index of the blending function is related 
 * to how many line segments are used to draw the spline for two sample
 * points (that is in a section of the spline).
 */

{
  for (int i = 0; i < numberOfLines; i++)
    {
      double u = (double) (i + 1) / numberOfLines;
      firstBlend[0][i] = pow (1.0 - u, 3);
      firstBlend[3][i] = pow (u, 3) / 6.0;
      firstBlend[2][i] = ((3.0 / 2.0) - (11.0 * u / 12.0))
	* pow (u, 2);
      firstBlend[1][i] = 1.0 - firstBlend[0][i]
	- firstBlend[2][i]
	- firstBlend[3][i];

      secondBlend[0][i] = firstBlend[0][i] / 4.0;
      secondBlend[3][i] = firstBlend[3][i];
      secondBlend[2][i] = (((1 - u) * u + 1.0) * u + 1.0 / 3.0) / 2.0;
      secondBlend[1][i] = 1.0 - secondBlend[0][i]
	- secondBlend[2][i]
	- secondBlend[3][i];

      mainBlend[0][i] = firstBlend[0][i] / 6.0;
      mainBlend[3][i] = firstBlend[3][i];
      mainBlend[2][i] = secondBlend[2][i];
      mainBlend[1][i] = 1.0 - mainBlend[0][i]
	- mainBlend[2][i]
	- mainBlend[3][i];

      int j = numberOfLines - i - 2;
/*  -2 because both i and j are array subscripts and go from 0 rather than 
 *  from 1, as they do in the algorithm. 
 *
 *  Having j >= 0 lets through all values of j except the last, which
 *  is the (numberOfLines - 1)th index, like the algorithm does.
 */
      if (j >= 0)
	{
	  penultimateBlend[0][j] = secondBlend[3][i];
	  penultimateBlend[1][j] = secondBlend[2][i];
	  penultimateBlend[2][j] = secondBlend[1][i];
	  penultimateBlend[3][j] = secondBlend[0][i];

	  lastBlend[0][j] = firstBlend[3][i];
	  lastBlend[1][j] = firstBlend[2][i];
	  lastBlend[2][j] = firstBlend[1][i];
	  lastBlend[3][j] = firstBlend[0][i];
	}
    }

  penultimateBlend[0][numberOfLines - 1] = 0.0;
  penultimateBlend[1][numberOfLines - 1] = 1.0 / 6.0;
  penultimateBlend[2][numberOfLines - 1] = 7.0 / 12.0;
  penultimateBlend[3][numberOfLines - 1] = 1.0 / 4.0;

  lastBlend[0][numberOfLines - 1] = 0.0;
  lastBlend[1][numberOfLines - 1] = 0.0;
  lastBlend[2][numberOfLines - 1] = 0.0;
  lastBlend[3][numberOfLines - 1] = 1.0;
}

/*************************************************************************
 *  This function draws the spline from the startPoint (which will be the
 *  last calculated spline point for the previous sample section), and
 *  draws numberOfLines spline segments.
 *
 *  It's arguments are:
 *
 *  1) dc -- the device on which to draw the spline
 *  2) samplePoints[4] -- the 4 element array of sample points for the
 *                        current spline section
 *  3) numberOfLines -- number of segment lines per spline section
 *  4) blendFunction -- the particular blend function array which applies
 *                      for this section of the spline. There are
 *  		        5 blend function arrays, which are initialised
 * 		        in function initBSpline.
 *  5) startPoint -- the 1 element array which holds the x and y 
 *                   coordinates of the point where this spline is to
 * 		     drawn from -- this is updated in the function such
 *		     that the coordinates are always the end coordinates
 *		     of the spline so far.
 */

static void 
MakeCurve (wxbDC * dc, wxPoint samplePoints[],
	   int numberOfLines,
	   double *blendFunction[],
	   wxPoint startPoint[])
{
  for (int j = 0; j < numberOfLines; j++)
    {
      double x = 0.0;
      double y = 0.0;
      for (int i = 0; i < 4; i++)
	{
	  x = x + samplePoints[i].x * blendFunction[i][j];
	  y = y + samplePoints[i].y * blendFunction[i][j];
	}
      dc->DrawLine ((float) startPoint[0].x, (float) startPoint[0].y, (float) x, (float) y);
      startPoint[0].x = (float) x;
      startPoint[0].y = (float) y;
    }
}

/*************************************************************************
 *  This function advances the sample points, being used to determine
 *  the spline curve, by 1. The function nextSamplePoint (below) gets
 *  the new sample point.
 *
 *  It's argument is:
 *
 *  1) samplePoints[4] -- the 4 element array of sample points for the
 *                        current spline section
 */

static void 
nextSection (wxPoint samplePoints[])
{
  for (int i = 0; i < 3; i++)
    {
      samplePoints[i].x = samplePoints[i + 1].x;
      samplePoints[i].y = samplePoints[i + 1].y;
    }
}

/*************************************************************************
 *  This function gets the next sample point in order to calculate
 *  the next section of the spline curve.
 *
 *  It's arguments are:
 *
 *  1) currentPoint -- a variable which holds the index number to the
 *                     current array element in points (the array in which all
 *		       the sample points are held). The current array element
 *		       is the sample point that we have got to in points[].
 *
 *  2) samplePoints -- the 4 element array of sample points for the
 *                     current spline section
 *   3) points -- the nPoints size array, that hold the sample points
 *	 
 */

static void 
nextSamplePoint (int &currentPoint,
		 wxPoint samplePoints[], wxPoint points[])
{

// move on to next sample point, and put this as the 4th element in
  // the samplePoints array
  currentPoint = currentPoint + 1;
  samplePoints[3].x = points[currentPoint].x;
  samplePoints[3].y = points[currentPoint].y;
}

#if USE_POSTSCRIPT
#include "wx_dcps.h"

void wxPostScriptDC::
DrawSpline (float x1, float y1, float x2, float y2, float x3, float y3)
{
  wxDC::DrawSpline (x1, y1, x2, y2, x3, y3);
}

void wxPostScriptDC::DrawSpline (wxList * points)
{
  wxDC::DrawSpline (points);
}

void wxPostScriptDC::DrawSpline (int n, wxPoint points[])
{
  wxDC::DrawSpline (n, points);
}

#endif
