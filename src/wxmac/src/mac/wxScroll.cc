///////////////////////////////////////////////////////////////////////////////
// File:	wxScroll.cc
// Purpose:	wxScroll (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxScroll.h"
#include "wx_utils.h"
#include "wx_win.h"

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxScroll::wxScroll // root scroll
(
	wxWindow*		scrollWindow,
	wxScrollData*	scrollData
) :
	cScrollWindow (scrollWindow),
	cScrollData (scrollData),
	cParentScroll (NULL),
	cScrolls (wxList(wxList::kNoDestroyData))
{
	WXGC_IGNORE(scrollWindow);
	if (!scrollWindow) wxFatalError("No scroll window for constructing scroll.");
	if (!scrollData) wxFatalError("No scroll data for constructing root scroll.");
}

//-----------------------------------------------------------------------------
wxScroll::wxScroll // child scroll
(
	wxWindow*	scrollWindow,
	wxWindow*	parentScrollWindow
) :
	cScrollWindow (scrollWindow),
	cScrollData (NULL),
	cScrolls (wxList(wxList::kNoDestroyData))
{
	WXGC_IGNORE(scrollWindow);
	if (!scrollWindow)
		wxFatalError("No scroll window for constructing scroll.");
	if (!parentScrollWindow)
		wxFatalError("No parent scroll window for constructing scroll.");

	cParentScroll = parentScrollWindow->GetScroll();
	if (!cParentScroll)
		wxFatalError("No parent scroll for constructing scroll.");

	cParentScroll->cScrolls.Append(this);
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxScroll::~wxScroll(void)	// destructor
{
	// NOTE: Only the wxWindow object that owns this wxScroll should invoke its deletion.
	//       Hence, owner will NULL out its cScroll link to this object

	wxNode* childScrollNode = cScrolls.First();
	while (childScrollNode)
	{
		wxScroll* childScroll = (wxScroll*)childScrollNode->Data();
		childScroll->cParentScroll = cParentScroll;
		if (cParentScroll) cParentScroll->cScrolls.Append(this);
		childScrollNode = childScrollNode->Next();
	}

	delete cScrollData;

	if (cParentScroll) cParentScroll->OnDeleteChildScroll(this);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Getter and setter methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxScrollData* wxScroll::GetScrollData (void)
{
	wxScroll* rootScroll = RootScroll();
	wxScrollData* rootScrollData = rootScroll->cScrollData;

	if (!rootScrollData) wxFatalError("No scroll data for scroll.");

	return rootScrollData;
}

//-----------------------------------------------------------------------------
void wxScroll::SetScrollData
(
	wxScrollData*		newScrollData,
	wxWhatScrollData	whatScrollData, // items to be changed
	wxWindow*			iniatorWindow
)
{
	wxScrollData* scrollData = GetScrollData();
	scrollData->SetValue(newScrollData, whatScrollData);
	RootScroll()->OnSetScrollData(scrollData, whatScrollData, iniatorWindow);
}

//-----------------------------------------------------------------------------
void wxScroll::SetScrollData
(
	int 				value,			// value for items to be changed
	wxWhatScrollData	whatScrollData, // items to be changed
	wxWindow*			iniatorWindow
)
{
	wxScrollData* scrollData = GetScrollData();
	scrollData->SetValue(value, whatScrollData);
	RootScroll()->OnSetScrollData(scrollData, whatScrollData, iniatorWindow);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Scroll tree methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
wxScroll* wxScroll::RootScroll(void)
{
	wxScroll* result = this;
	while (result->cParentScroll) result = result->cParentScroll;
	return result;
}

//-----------------------------------------------------------------------------
void wxScroll::AddChildScrollWindow(wxWindow* childScrollWindow)
{
	if (!childScrollWindow)
		wxFatalError("No childScrollWindow for AddChildScrollWindow.");

	wxScroll* childScroll = childScrollWindow->GetScroll();
	if (!childScroll)
		wxFatalError("No childScroll for AddChildScrollWindow.");

	if (childScroll->cParentScroll)
		wxFatalError("ChildScroll not root for AddChildScrollWindow.");
	childScroll->cParentScroll = this;

	cScrolls.Append(childScroll);

	if (childScroll->cScrollData)
	{
		delete childScroll->cScrollData;
		childScroll->cScrollData = NULL;
	}

	wxScrollData* scrollData = GetScrollData();
	childScroll->OnSetScrollData(scrollData, wxWhatScrollData::wxAll, NULL);
}

//-----------------------------------------------------------------------------
void wxScroll::OnDeleteChildScroll(wxScroll* childScroll)
{
	cScrolls.OnDeleteObject(childScroll);
}

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//-----------------------------------------------------------------------------
void wxScroll::OnSetScrollData
(
	wxScrollData*		scrollData,
	wxWhatScrollData	whatScrollData, // items to be changed
	wxWindow*			iniatorWindow
)
{
	wxNode* childScrollNode = cScrolls.First();
	while (childScrollNode)
	{
		wxScroll* childScroll = (wxScroll*)childScrollNode->Data();
		childScroll->OnSetScrollData(scrollData, whatScrollData, iniatorWindow);
		childScrollNode = childScrollNode->Next();
	}

	cScrollWindow->SetScrollData(scrollData, whatScrollData, iniatorWindow);
}
