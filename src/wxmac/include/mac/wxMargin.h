///////////////////////////////////////////////////////////////////////////////
// File:	wxMargin.h
// Purpose:	Margin (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxMarginh
#define wxMarginh

class Direction;

class wxMargin
{
  protected:
	int left;
	int top;
	int right;
	int bottom;

  public:
  	wxMargin(int margin = 0);
	wxMargin(int margin, Direction direction);
	wxMargin(const wxMargin& margin);
  	~wxMargin(void);

  	wxMargin& operator +=(wxMargin margin);

	void SetMargin(wxMargin margin, Direction direction);
	void SetMargin(int margin, Direction direction);
	int Offset(Direction direction);
};

#endif // wxMarginh