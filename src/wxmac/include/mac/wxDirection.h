///////////////////////////////////////////////////////////////////////////////
// File:	wxDirection.h
// Purpose:	Direction (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#ifndef wxDirectionh
#define wxDirectionh

class Direction
{
  public:
  	enum {wxTop = 1, wxLeft = 2, wxBottom = 4, wxRight = 8,
  			wxVertical = wxTop | wxBottom,
  			wxHorizontal = wxLeft | wxRight,
  			wxAll = wxVertical | wxHorizontal};

  protected:
	int v;

  public:
  	Direction(void) {v = 0;}
	Direction(int i) {v = i;}
  	operator int() {return v;}

	Direction operator& (Direction aDirection) { return this->v & aDirection.v; } // 95-02-20
};

#endif // wxDirectionh