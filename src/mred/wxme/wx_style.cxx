/*
 * File:        wx_style.cc
 * Purpose:     wxStyle and wxStyleList implementation
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 1995, Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 */

#include "wx_gdi.h"
#include "wx_main.h"
#include "wx_dialg.h"
#ifndef OLD_WXWINDOWS
#include "wx_cmdlg.h"
#endif
#include "wx_style.h"
#include "wx_mtype.h"
#include "wx_medio.h"
#include "wx_ptreq.h"
#include <string.h>

#ifdef wx_x
static int defaultSize = 12;
#endif
#ifdef wx_msw
static int defaultSize = 12;
#endif
#ifdef wx_mac
static int defaultSize = 12;
#endif

#if !defined(wx_xt) || defined(WXME_FOR_MRED)
#define FONT_DIRECTORY wxTheFontNameDirectory
#else
#define FONT_DIRECTORY (*wxTheFontNameDirectory)
#endif

wxStyleList *wxTheStyleList;

static wxColour *whiteColour, *blackColour;

void wxInitStyles(void)
{
  if (wxTheStyleList)
    return;

  whiteColour = wxTheColourDatabase->FindColour("WHITE");
  blackColour = wxTheColourDatabase->FindColour("BLACK");

#if USE_RESOURCES
  wxGetResource(wxTheApp->wx_class, "defaultFontSize", &defaultSize);
#endif

  wxTheStyleList = new wxStyleList;
}

void wxMultColour::Get(float *rf, float *gf, float *bf)
{
  *rf = r;
  *gf = g;
  *bf = b;
}

void wxMultColour::Set(float rf, float gf, float bf)
{
  r = rf;
  g = gf;
  b = bf;
}

void wxAddColour::Get(short *rf, short *gf, short *bf)
{
  *rf = r;
  *gf = g;
  *bf = b;
}

void wxAddColour::Set(short rf, short gf, short bf)
{
  r = rf;
  g = gf;
  b = bf;
}

wxStyleDelta::wxStyleDelta(int changeCommand, int param) 
: wxObject(WXGC_NO_CLEANUP)
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_STYLE_DELTA;
#endif

  face = NULL;
    
  SetDelta(wxCHANGE_NOTHING);
  SetDelta(changeCommand, param);
}

wxStyleDelta::~wxStyleDelta()
{
  if (face)
    delete[] face;
}

wxStyleDelta *wxStyleDelta::SetDelta(int changeCommand, int param)
{
  switch (changeCommand) {
  case wxCHANGE_NOTHING:
    family = wxBASE;
    if (face) {
      delete[] face;
      face = NULL;
    }
    sizeMult = 1;
    sizeAdd = 0;
    weightOn = wxBASE;
    weightOff = wxBASE;
    styleOn = wxBASE;
    styleOff = wxBASE;
    underlinedOn = underlinedOff = FALSE;
    transparentTextBackingOn = transparentTextBackingOff = FALSE;
    foregroundMult.Set(1, 1, 1);
    foregroundAdd.Set(0, 0, 0);
    backgroundMult.Set(1, 1, 1);
    backgroundAdd.Set(0, 0, 0);
    alignmentOn = wxBASE;
    alignmentOff = wxBASE;
    break;
  case wxCHANGE_STYLE:
    styleOn = param;
    styleOff = wxBASE;
    break;
  case wxCHANGE_WEIGHT:
    weightOn = param;
    weightOff = wxBASE;
    break;
  case wxCHANGE_UNDERLINE:
    underlinedOn = param;
    underlinedOff = !param;
    break;
  case wxCHANGE_SIZE:
    sizeMult = 0;
    sizeAdd = param;
    break;
  case wxCHANGE_FAMILY:
    family = param;
    if (face) {
      delete[] face;
      face = NULL;
    }
    break;
  case wxCHANGE_ALIGNMENT:
    alignmentOn = param;
    alignmentOff = wxBASE;
    break;
  case wxCHANGE_BOLD:
    weightOn = wxBOLD;
    weightOff = wxBASE;
    break;
  case wxCHANGE_ITALIC:
    styleOn = wxITALIC;
    styleOff = wxBASE;
    break;
  case wxCHANGE_TOGGLE_STYLE:
    styleOn = param;
    styleOff = param;
    break;
  case wxCHANGE_TOGGLE_WEIGHT:
    weightOn = param;
    weightOff = param;
    break;
  case wxCHANGE_TOGGLE_UNDERLINE:
    underlinedOn = TRUE;
    underlinedOff = TRUE;
    break;
  case wxCHANGE_BIGGER:
    sizeMult = 1;
    sizeAdd = param;
    break;
  case wxCHANGE_SMALLER:
    sizeMult = 1;
    sizeAdd = -param;
    break;
  case wxCHANGE_NORMAL:
    family = wxDEFAULT;
    if (face) {
      delete[] face;
      face = NULL;
    }
    sizeMult = 0;
    sizeAdd = defaultSize;
    weightOn = wxNORMAL;
    weightOff = wxBASE;
    styleOn = wxNORMAL;
    styleOff = wxBASE;
    underlinedOn = FALSE;
    underlinedOff = TRUE;
    alignmentOn = wxALIGN_BOTTOM;
    alignmentOff = wxBASE;
    /* fall through ... */
  case wxCHANGE_NORMAL_COLOUR: /* ^^ falls through */
    foregroundMult.Set(0, 0, 0);
    foregroundAdd.Set(0, 0, 0);
    backgroundMult.Set(0, 0, 0);
    backgroundAdd.Set(255, 255, 255);
    break;
  }

  return this;
}

wxStyleDelta *wxStyleDelta::SetDeltaFace(char *name)
{
  if (face)
    delete[] face;
  face = copystring(name);
#ifdef NO_GENERAL_FONTS
  family = wxBASE;
#else
  int id = FONT_DIRECTORY.FindOrCreateFontId(name, wxDEFAULT);
  family = FONT_DIRECTORY.GetFamily(id);
#endif
  return this;
}

wxStyleDelta *wxStyleDelta::SetDeltaBackground(char *name)
{
  wxColour *c;

  transparentTextBackingOn = FALSE;
  transparentTextBackingOff = TRUE;

  if ((c = wxTheColourDatabase->FindColour(name)))
    SetDeltaBackground(*c);

  return this;
}

wxStyleDelta *wxStyleDelta::SetDeltaBackground(wxColour& colour)
{
  unsigned char r, g, b;

  transparentTextBackingOn = FALSE;
  transparentTextBackingOff = TRUE;

  backgroundMult.Set(0, 0, 0);
  colour.Get(&r, &g, &b);
  backgroundAdd.Set(r, g, b);

  return this;
}

wxStyleDelta *wxStyleDelta::SetDeltaForeground(char *name)
{
  wxColour *c;

  if ((c = wxTheColourDatabase->FindColour(name)))
    SetDeltaForeground(*c);
  return this;
}

wxStyleDelta *wxStyleDelta::SetDeltaForeground(wxColour& colour)
{
  unsigned char r, g, b;

  foregroundMult.Set(0, 0, 0);
  colour.Get(&r, &g, &b);
  foregroundAdd.Set(r, g, b);
  return this;
}

Bool wxStyleDelta::Collapse(wxStyleDelta &deltaIn)
{
  float ambr, ambb, ambg, amfr, amfb, amfg;
  float bmbr, bmbb, bmbg, bmfr, bmfb, bmfg;
  short aabr, aabb, aabg, aafr, aafb, aafg;
  short babr, babb, babg, bafr, bafb, bafg;

  /* If collapsing possible? */
  /* It may not be if add & multiply sequence occurs, */
  /* or certain toggling settings conflict or */
  if (sizeMult && sizeMult != 1.0 && deltaIn.sizeAdd != 0)
    return FALSE;

  foregroundMult.Get(&amfr, &amfb, &amfg);
  backgroundMult.Get(&ambr, &ambb, &ambg);
  deltaIn.foregroundAdd.Get(&bafr, &bafb, &bafg);
  deltaIn.backgroundAdd.Get(&babr, &babb, &babg);
  if ((amfr && amfr != 1.0 && bafr != 0)
      || (amfg && amfg != 1.0 && bafg != 0)
      || (amfb && amfb != 1.0 && bafb != 0)
      || (ambr && ambr != 1.0 && babr != 0)
      || (ambg && ambg != 1.0 && babg != 0)
      || (ambb && ambb != 1.0 && babb != 0))
    return FALSE;

  // Cases: simple or double toggle
  //        no further change
  //        formerly no change
  //        style definitely on
  //        style definitely off
  if (!((styleOn == deltaIn.styleOn && styleOff == deltaIn.styleOff)
	|| (styleOn == wxBASE && styleOff == wxBASE)
	|| (deltaIn.styleOn == wxBASE && deltaIn.styleOff == wxBASE)
	|| (styleOn == wxBASE && styleOff != wxBASE)
	|| (styleOff == wxBASE && styleOn != wxBASE)))
    return FALSE;
  if (!((weightOn == deltaIn.weightOn && weightOff == deltaIn.weightOff)
	|| (weightOn == wxBASE && weightOff == wxBASE)
	|| (weightOn == wxBASE && weightOff != wxBASE)
	|| (weightOff == wxBASE && weightOn != wxBASE)))
    return FALSE;
  if (!((alignmentOn == deltaIn.alignmentOn 
	 && alignmentOff == deltaIn.alignmentOff)
	|| (alignmentOn == wxBASE && alignmentOff == wxBASE)
	|| (alignmentOn == wxBASE && alignmentOff != wxBASE)
	|| (alignmentOff == wxBASE && alignmentOn != wxBASE)))
    return FALSE;

  if (!((underlinedOn == deltaIn.underlinedOn
	 && underlinedOff == deltaIn.underlinedOff)
	|| (!underlinedOn && !underlinedOff)
	|| (!deltaIn.underlinedOn && !deltaIn.underlinedOff)
	|| (!underlinedOn && underlinedOff)
	|| (!underlinedOff && underlinedOn)))
    return FALSE;

  if (!((transparentTextBackingOn == deltaIn.transparentTextBackingOn
	 && transparentTextBackingOff == deltaIn.transparentTextBackingOff)
	|| (!transparentTextBackingOn && !transparentTextBackingOff)
	|| (!deltaIn.transparentTextBackingOn && !deltaIn.transparentTextBackingOff)
	|| (!transparentTextBackingOn && transparentTextBackingOff)
	|| (!transparentTextBackingOff && transparentTextBackingOn)))
    return FALSE;

  /* Collapsing is possible. */

  deltaIn.foregroundMult.Get(&bmfr, &bmfb, &bmfg);
  deltaIn.backgroundMult.Get(&bmbr, &bmbb, &bmbg);
  foregroundAdd.Get(&aafr, &aafb, &aafg);
  backgroundAdd.Get(&aabr, &aabb, &aabg);
  
  sizeAdd += (int)(sizeMult * deltaIn.sizeAdd);
  sizeMult *= deltaIn.sizeMult;

  foregroundMult.Set(amfr * bmfr, amfb * bmfb, amfg * bmfg);
  backgroundMult.Set(ambr * bmbr, ambb * bmbb, ambg * bmbg);
  foregroundAdd.Set(aafr + (int)(amfr * bafr), 
		    aafb + (int)(amfb * bafb), 
		    aafg + (int)(amfg * bafg));
  backgroundAdd.Set(aabr + (int)(ambr * babr), 
		    aabb + (int)(ambb * babb), 
		    aabg + (int)(ambg * babg));

  if (family == wxBASE) {
    family = deltaIn.family;
    if (deltaIn.face)
      face = copystring(deltaIn.face);
  }

  if (styleOn == wxBASE && styleOff == wxBASE) {
    styleOff = deltaIn.styleOff;
    styleOn = deltaIn.styleOn;
  } else if (styleOn != wxBASE && styleOff != wxBASE) {
    if (deltaIn.styleOn != wxBASE || deltaIn.styleOff != wxBASE
	&& styleOn == styleOff)
      styleOn = styleOff = wxBASE; // Double toggle
  }
  if (weightOn == wxBASE && weightOff == wxBASE) {
    weightOff = deltaIn.weightOff;
    weightOn = deltaIn.weightOn;
  } else if (weightOn != wxBASE && weightOff != wxBASE) {
    if (deltaIn.weightOn != wxBASE || deltaIn.weightOff != wxBASE
	&& weightOn == weightOff)
      weightOn = weightOff = wxBASE; // Double toggle
  }
  if (alignmentOn == wxBASE && alignmentOff == wxBASE) {
    alignmentOff = deltaIn.alignmentOff;
    alignmentOn = deltaIn.alignmentOn;
  } else if (alignmentOn != wxBASE && alignmentOff != wxBASE) {
    if (deltaIn.alignmentOn != wxBASE || deltaIn.alignmentOff != wxBASE
	&& alignmentOn == alignmentOff)
      alignmentOn = alignmentOff = wxBASE; // Double toggle
  }
  if (!underlinedOn && !underlinedOff) {
    underlinedOn = deltaIn.underlinedOn;
    underlinedOff = deltaIn.underlinedOff;
  } else if (underlinedOn && underlinedOff) {
    if (deltaIn.underlinedOn && deltaIn.underlinedOff)
      underlinedOn = underlinedOff = FALSE;
  }
  if (!transparentTextBackingOn && !transparentTextBackingOff) {
    transparentTextBackingOn = deltaIn.transparentTextBackingOn;
    transparentTextBackingOff = deltaIn.transparentTextBackingOff;
  } else if (transparentTextBackingOn && transparentTextBackingOff) {
    if (deltaIn.transparentTextBackingOn && deltaIn.transparentTextBackingOff)
      transparentTextBackingOn = transparentTextBackingOff = FALSE;
  }

  return TRUE;
}

Bool wxStyleDelta::Equal(wxStyleDelta &deltaIn)
{
  float ambr, ambb, ambg, amfr, amfb, amfg;
  float bmbr, bmbb, bmbg, bmfr, bmfb, bmfg;
  short aabr, aabb, aabg, aafr, aafb, aafg;
  short babr, babb, babg, bafr, bafb, bafg;
  
  foregroundMult.Get(&amfr, &amfb, &amfg);
  backgroundMult.Get(&ambr, &ambb, &ambg);
  foregroundAdd.Get(&aafr, &aafb, &aafg);
  backgroundAdd.Get(&aabr, &aabb, &aabg);
  deltaIn.foregroundMult.Get(&bmfr, &bmfb, &bmfg);
  deltaIn.backgroundMult.Get(&bmbr, &bmbb, &bmbg);
  deltaIn.foregroundAdd.Get(&bafr, &bafb, &bafg);
  deltaIn.backgroundAdd.Get(&babr, &babb, &babg);

  return (((!face && !deltaIn.face && family == deltaIn.family)
	   || (face && deltaIn.face && !strcmp(face, deltaIn.face)))
	  && sizeMult == deltaIn.sizeMult
	  && sizeAdd == deltaIn.sizeAdd
	  && weightOn == deltaIn.weightOn
	  && weightOff == deltaIn.weightOff
	  && styleOn == deltaIn.styleOn
	  && styleOff == deltaIn.styleOff
	  && underlinedOn == deltaIn.underlinedOn
	  && underlinedOff == deltaIn.underlinedOff
	  && transparentTextBackingOn == deltaIn.transparentTextBackingOn
	  && transparentTextBackingOff == deltaIn.transparentTextBackingOff
	  && amfr == bmfr && amfb == bmfb && amfg == bmfg
	  && aafr == bafr && aafb == bafb && aafg == bafg
	  && ambr == bmbr && ambb == bmbb && ambg == bmbg
	  && aabr == babr && aabb == babb && aabg == babg
	  && alignmentOn == deltaIn.alignmentOn
	  && alignmentOff == deltaIn.alignmentOff);
}

void wxStyleDelta::Copy(wxStyleDelta *in)
{
#define DCOPY(x) x = in->x
  DCOPY(family);
  DCOPY(face);
  DCOPY(sizeMult);
  DCOPY(sizeAdd);
  DCOPY(weightOn);
  DCOPY(weightOff);
  DCOPY(styleOn);
  DCOPY(styleOff);
  DCOPY(underlinedOn);
  DCOPY(underlinedOff);
  DCOPY(transparentTextBackingOn);
  DCOPY(transparentTextBackingOff);
  DCOPY(foregroundMult.r);
  DCOPY(foregroundMult.g);
  DCOPY(foregroundMult.b);
  DCOPY(foregroundAdd.r);
  DCOPY(foregroundAdd.g);
  DCOPY(foregroundAdd.b);
  DCOPY(backgroundMult.r);
  DCOPY(backgroundMult.g);
  DCOPY(backgroundMult.b);
  DCOPY(backgroundAdd.r);
  DCOPY(backgroundAdd.g);
  DCOPY(backgroundAdd.b);
  DCOPY(alignmentOn);
  DCOPY(alignmentOff);
}

/***************************************************************/

static unsigned char ColourNum(float v)
{
  if (v < 0)
    return 0;
  else if (v > 255)
    return 255;
  else
    return (unsigned char)v;
}

wxStyle::wxStyle()
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_STYLE;
#endif

  textMetricDC = NULL;

  WXGC_IGNORE(styleList);
  WXGC_IGNORE(baseStyle);
  WXGC_IGNORE(textMetricDC);

  joinStyle = FALSE;
}

wxStyle::~wxStyle()
{
  if (!joinStyle)
    delete u.delta;
}

void wxStyle::Update(wxStyle *basic, wxStyle *target, 
		     Bool propogate, Bool topLevel)
{
  int size;
  int fontid;
  int style, weight;
  unsigned char r, g, b;
  float rm, gm, bm;
  short rp, gp, bp; 
  Bool match;
  wxNode *node;
  Bool underlined;
  wxStyle *base;

  base = baseStyle;
  if (basic) {
    if (PTREQ(base, styleList->BasicStyle())) {
      base = basic;
    } else {
      base->Update(basic, target, FALSE, FALSE);
      base = target;
    }
  }

  if (!target)
    target = this;

  if (joinStyle) {
    if (!PTREQ(u.shiftStyle, styleList->BasicStyle()))
      u.shiftStyle->Update(base, target, FALSE, topLevel);
    return;
  }

  size = (int)(u.delta->sizeMult * base->font->GetPointSize());
  size += u.delta->sizeAdd;
  if (size <= 0)
    size = 1;

  if (!u.delta->face && u.delta->family == wxBASE) {
#ifndef NO_GENERAL_FONTS
    fontid = base->font->GetFontId();
#else
    fontid = base->font->GetFamily();
#endif
  }
#ifndef NO_GENERAL_FONTS
  else if (u.delta->face)
    fontid = FONT_DIRECTORY.FindOrCreateFontId(u.delta->face, 
					       u.delta->family);
#endif
  else
    fontid = u.delta->family;

  style = base->font->GetStyle();
  match = (style == u.delta->styleOff);
  if (match)
    style = wxNORMAL;
  if (!match || (match && u.delta->styleOn != u.delta->styleOff))
    if (u.delta->styleOn != wxBASE)
      style = u.delta->styleOn;

  weight = base->font->GetWeight();
  match = (weight == u.delta->weightOff);
  if (match)
    weight = wxNORMAL;
  if (!match || (match && u.delta->weightOn != u.delta->weightOff))
    if (u.delta->weightOn != wxBASE)
      weight = u.delta->weightOn;

  target->alignment = base->alignment;
  match = (target->alignment == u.delta->alignmentOff);
  if (match)
    target->alignment = wxALIGN_BOTTOM;
  if (!match || (match && u.delta->alignmentOn != u.delta->alignmentOff))
    if (u.delta->alignmentOn != wxBASE)
      target->alignment = u.delta->alignmentOn;

  if (u.delta->underlinedOff && u.delta->underlinedOn)
    underlined = !base->font->GetUnderlined();
  else if (u.delta->underlinedOff)
    underlined = FALSE;
  else if (u.delta->underlinedOn)
    underlined = TRUE;
  else
    underlined = base->font->GetUnderlined();
  
  target->font = wxTheFontList->FindOrCreateFont(size, fontid,
						 style, weight, underlined);

  target->textMetricDC = NULL;

  if (u.delta->transparentTextBackingOff && u.delta->transparentTextBackingOn)
    transText = !base->transText;
  else if (u.delta->transparentTextBackingOff)
    transText = FALSE;
  else if (u.delta->transparentTextBackingOn)
    transText = TRUE;
  else
    transText = base->transText;
  
  base->foreground.Get(&r, &g, &b);
  u.delta->foregroundMult.Get(&rm, &gm, &bm);
  u.delta->foregroundAdd.Get(&rp, &gp, &bp);
  r = ColourNum(r * rm + rp);
  g = ColourNum(g * gm + gp);
  b = ColourNum(b * bm + bp);
  target->foreground.Set(r, g, b);

  base->background.Get(&r, &g, &b);
  u.delta->backgroundMult.Get(&rm, &gm, &bm);
  u.delta->backgroundAdd.Get(&rp, &gp, &bp);
  r = ColourNum(r * rm + rp);
  g = ColourNum(g * gm + gp);
  b = ColourNum(b * bm + bp);
  target->background.Set(r, g, b);

  target->pen = wxThePenList->FindOrCreatePen(&foreground, 0, wxSOLID);
  target->brush = wxTheBrushList->FindOrCreateBrush(&background, wxSOLID);

  if (propogate)
    for (node = children.First(); node; node = node->Next())
      ((wxStyle *)node->Data())->Update(NULL, NULL, TRUE, FALSE);

  styleList->StyleWasChanged(target);
  if (topLevel)
    styleList->StyleWasChanged(NULL);
}

char *wxStyle::GetName()
{
  return name;
}

int wxStyle::GetFamily()
{
  return font->GetFamily();
}

char *wxStyle::GetFace()
{
#ifndef NO_GENERAL_FONTS
#ifndef WXME_FOR_MRED
  return font->GetFaceName();
#else
  return font->GetFaceString();
#endif
#else
  return font->GetFamilyString();
#endif
}

wxFont *wxStyle::GetFont()
{
  return font;
}

int wxStyle::GetSize()
{
  return font->GetPointSize();
}

int wxStyle::GetWeight()
{
  return font->GetWeight();
}

int wxStyle::GetStyle()
{
  return font->GetStyle();
}

Bool wxStyle::GetUnderlined()
{
  return font->GetUnderlined();
}

Bool wxStyle::GetTransparentTextBacking()
{
  return transText;
}

wxColour &wxStyle::GetForeground()
{
  return foreground;
}

wxColour &wxStyle::GetBackground()
{
  return background;
}

int wxStyle::GetAlignment()
{
  return alignment;
}

Bool wxStyle::IsJoin()
{
  return joinStyle;
}

void wxStyle::GetDelta(wxStyleDelta &d)
{
  if (joinStyle)
    d.SetDelta(wxCHANGE_NOTHING);
  else
    d.Copy(u.delta);
}

void wxStyle::SetDelta(wxStyleDelta &d)
{
  if (joinStyle || PTREQ(this, styleList->BasicStyle()))
    return;

  u.delta->Copy(&d);
  Update();
}

wxStyle *wxStyle::GetShiftStyle()
{
  if (joinStyle)
    return u.shiftStyle;
  else
    return styleList->BasicStyle();
}

void wxStyle::SetShiftStyle(wxStyle *style)
{
  if (!joinStyle || (styleList->StyleToIndex(style) < 0))
    return;

  if (styleList->CheckForLoop(this, style))
    return;

  if (u.shiftStyle)
    u.shiftStyle->children.DeleteObject(this);
  style->children.Append(this);

  u.shiftStyle = style;
  styleList->StyleHasNewChild(style, this);

  Update();

  u.shiftStyle = style;
  Update();
}

wxStyle *wxStyle::GetBaseStyle(void)
{
  return baseStyle;
}

void wxStyle::SetBaseStyle(wxStyle *style)
{
  if (PTREQ(this, styleList->BasicStyle()))
    return;

  if (!style)
    style = styleList->BasicStyle();
  else
    if (styleList->StyleToIndex(style) < 0)
      return;

  if (styleList->CheckForLoop(this, style))
    return;

  if (baseStyle)
    baseStyle->children.DeleteObject(this);

  baseStyle = style;
  style->children.Append(this);

  styleList->StyleHasNewChild(style, this);

  Update();
}

void wxStyle::SwitchTo(wxDC *dc, wxStyle *oldStyle)
{
  unsigned char afr, afg, afb, bfr, bfg, bfb;
  unsigned char abr, abg, abb, bbr, bbg, bbb;

  if (oldStyle) {
    oldStyle->foreground.Get(&afr, &afg, &afb);
    foreground.Get(&bfr, &bfg, &bfb);
    oldStyle->background.Get(&abr, &abg, &abb);
    background.Get(&bbr, &bbg, &bbb);
  }

  if (!oldStyle || oldStyle->font != font)
    dc->SetFont(font);
  if (!oldStyle || afr != bfr || afb != bfb || afg != bfg)
    dc->SetTextForeground(&foreground);
  if (!oldStyle || abr != bbr || abb != bbb || abg != bbg)
    dc->SetTextBackground(&background);
  if (!oldStyle || oldStyle->pen != pen)
    dc->SetPen(pen);
  if (!oldStyle || oldStyle->transText != transText)
    dc->SetBackgroundMode(transText ? wxTRANSPARENT : wxSOLID);
}

void wxStyle::ResetTextMetrics(wxDC *dc)
{
  textMetricDC = dc;
#ifdef BROKEN_GET_TEXT_EXTENT 
  dc->SetFont(style->GetFont());
#endif
  dc->GetTextExtent(" ", &textWidth, &textHeight, &textDescent, &textSpace, font);
}

float wxStyle::GetTextWidth(wxDC *dc)
{
  if (dc != textMetricDC)
    ResetTextMetrics(dc);

  return textWidth;
}

float wxStyle::GetTextHeight(wxDC *dc)
{
  if (dc != textMetricDC)
    ResetTextMetrics(dc);

  return textHeight;
}

float wxStyle::GetTextDescent(wxDC *dc)
{
  if (dc != textMetricDC)
    ResetTextMetrics(dc);

  return textDescent;
}

float wxStyle::GetTextSpace(wxDC *dc)
{
  if (dc != textMetricDC)
    ResetTextMetrics(dc);

  return textSpace;
}

/***************************************************************/

class NotificationRec {
 public:
  wxStyleNotifyFunc f;
  void *data;
  long id;
};

static long nextNotifyId = 0;

wxStyleList::wxStyleList() : wxList()
{
#if USE_OLD_TYPE_SYSTEM
  __type = wxTYPE_STYLE_LIST;
#endif

  usage = 0;
  Clear();
}

void wxStyleList::Clear(void)
{
  wxNode *node;
  wxStyle *style;

  while ((node = First())) {
    style = (wxStyle *)node->Data();
    delete style;
    DeleteNode(node);
  }

  basic = new wxStyle;

  basic->styleList = this;

  basic->name = "Basic";
  basic->baseStyle = NULL;

  basic->u.delta = new wxStyleDelta;
  basic->u.delta->SetDelta(wxCHANGE_NORMAL);

  basic->font = wxTheFontList->FindOrCreateFont(defaultSize, wxDEFAULT,
						wxNORMAL, wxNORMAL);
  basic->foreground = *blackColour;
  basic->background = *whiteColour;
  basic->pen = wxThePenList->FindOrCreatePen(&basic->foreground, 0, wxSOLID);
  basic->brush = wxTheBrushList->FindOrCreateBrush(&basic->background, wxSOLID);
  basic->alignment = wxALIGN_BOTTOM;
  basic->transText = TRUE;

  Append(basic);

  notifications = new wxList();

  listId = 0;
  styleMap = NULL;
}

void wxStyleList::Copy(wxStyleList *other)
{
  wxNode *node;

  Clear();

  for (node = other->First(); node; node = node->Next()) {
    wxStyle *s = (wxStyle *)node->Data();
    Convert(s);
  }
}

wxStyleList::~wxStyleList()
{
  DeleteContents(TRUE);
}

wxStyle *wxStyleList::BasicStyle(void)
{
  return basic;
}

wxStyle *wxStyleList::FindOrCreateStyle(wxStyle *baseStyle, 
					wxStyleDelta *deltain)
{
  wxNode *node;
  wxStyle *style;
  wxStyleDelta delta;

  if (!baseStyle || (StyleToIndex(baseStyle) < 0))
    baseStyle = basic;

  /* Collapse the delta: */
  delta.Copy(deltain);
  while (!baseStyle->name && !baseStyle->joinStyle) {
    if (!delta.Collapse(*baseStyle->u.delta))
      break;
    baseStyle = baseStyle->baseStyle;
  }

  for (node = First(); node; node = node->Next()) {
    style = (wxStyle *)node->Data();
    if (!style->name
	&& !style->joinStyle
	&& PTREQ(style->baseStyle, baseStyle)
	&& delta.Equal(*style->u.delta))
      return style;
  }

  style = new wxStyle;

  style->styleList = this;

  style->name = NULL;

  style->u.delta = new wxStyleDelta;
  style->u.delta->Copy(&delta);

  style->baseStyle = baseStyle;
  baseStyle->children.Append(style);

  style->Update();

  Append(style);

  return style;
}

wxStyle *wxStyleList::FindOrCreateJoinStyle(wxStyle *baseStyle, 
					    wxStyle *shiftStyle)
{
  wxNode *node;
  wxStyle *style;

  if (!baseStyle || (StyleToIndex(baseStyle) < 0))
    baseStyle = basic;
  if (!shiftStyle || (StyleToIndex(shiftStyle) < 0))
    return baseStyle;

  for (node = First(); node; node = node->Next()) {
    style = (wxStyle *)node->Data();
    if (!style->name
	&& style->joinStyle
	&& PTREQ(style->baseStyle, baseStyle)
	&& PTREQ(style->u.shiftStyle, shiftStyle))
      return style;
  }

  style = new wxStyle;

  style->styleList = this;

  style->name = NULL;

  style->joinStyle = TRUE;

  WXGC_IGNORE(style->u.shiftStyle);
  style->u.shiftStyle = shiftStyle;
  shiftStyle->children.Append(style);

  style->baseStyle = baseStyle;
  baseStyle->children.Append(style);

  style->Update();

  Append(style);

  return style;
}

wxStyle *wxStyleList::FindNamedStyle(char *name)
{
  wxNode *node;
  wxStyle *style;

  for (node = First(); node; node = node->Next()) {
    style = (wxStyle *)node->Data();
    if (style->name && !strcmp(name, style->name))
      return style;
  }

  return NULL;
}

wxStyle *wxStyleList::DoNamedStyle(char *name, wxStyle *plainStyle, Bool replac)
{
  wxNode *node;
  wxStyle *style;

  if (!plainStyle || (StyleToIndex(plainStyle) < 0))
    plainStyle = basic;

  style = NULL;
  for (node = First(); node; node = node->Next()) {
    style = (wxStyle *)node->Data();
    if (style->name && !strcmp(name, style->name)) {
      if (!replac)
	return style;
      break;
    }
  }

  if (!node) {
    style = new wxStyle;
    style->name = copystring(name);
    style->styleList = this;
  } else {
    /* Can't replace basic style: */
    if (style == basic)
      return basic;

    style->baseStyle->children.DeleteObject(style);
    if (style->joinStyle)
      style->u.shiftStyle->children.DeleteObject(style);
  }

  if (plainStyle->joinStyle) {
    style->joinStyle = TRUE;
    style->u.shiftStyle = plainStyle->u.shiftStyle;
    style->u.shiftStyle->children.Append(style);
  } else {
    style->u.delta = new wxStyleDelta;
    if (PTRNE(plainStyle, basic))
      style->u.delta->Copy(plainStyle->u.delta);
  }

  if (PTREQ(plainStyle, basic))
    style->baseStyle = basic;
  else
    style->baseStyle = plainStyle->baseStyle;
  style->baseStyle->children.Append(style);

  style->Update();

  Append(style);

  return style;
}

wxStyle *wxStyleList::NewNamedStyle(char *name, wxStyle *plainStyle)
{
  return DoNamedStyle(name, plainStyle, FALSE);
}

wxStyle *wxStyleList::ReplaceNamedStyle(char *name, wxStyle *plainStyle)
{
  return DoNamedStyle(name, plainStyle, TRUE);
}

wxStyle *wxStyleList::Convert(wxStyle *style)
{
  wxStyle *base, *newstyle;

  if (StyleToIndex(style) >= 0)
    return style;

  if (style->name) {
    newstyle = FindNamedStyle(style->name);
    if (newstyle)
      return newstyle;
  }

  if (!style->baseStyle)
    base = BasicStyle();
  else
    base = Convert(style->baseStyle);

  if (style->joinStyle) {
    wxStyle *shift;

    shift = Convert(style->u.shiftStyle);
    newstyle = FindOrCreateJoinStyle(base, shift);
  } else
    newstyle = FindOrCreateStyle(base, style->u.delta);

  if (style->name)
    return NewNamedStyle(style->name, newstyle);
  else
    return newstyle;
}

void wxStyleList::StyleWasChanged(wxStyle *which)
{
  NotificationRec *rec;
  wxNode *node;

  for (node = notifications->First(); node; node = node->Next()) {
    rec = (NotificationRec *)node->Data();
    rec->f(which, rec->data);
  }
}

long wxStyleList::NotifyOnChange(wxStyleNotifyFunc f, void *data)
{
  NotificationRec *rec = new WXGC_ATOMIC NotificationRec;

  rec->f = f;
  rec->data = data;
  WXGC_IGNORE(rec->data);
  rec->id = nextNotifyId++;
  notifications->Append((wxObject *)rec);

  return rec->id;
}

void wxStyleList::ForgetNotification(long id)
{
  NotificationRec *rec;
  wxNode *node;
  
  for (node = notifications->First(); node; node = node->Next()) {
    rec = (NotificationRec *)node->Data();
    if (rec->id == id) {
      notifications->DeleteNode(node);
      delete rec;
      return;
    }
  }
}

Bool wxStyleList::CheckForLoop(wxStyle *s, wxStyle *p)
{
  if (PTREQ(p, s))
    return TRUE;

  if (!p->baseStyle)
    return FALSE;

  if (p->joinStyle) {
    if (CheckForLoop(s, p->baseStyle))
      return TRUE;
    return CheckForLoop(s, p->u.shiftStyle);
  } else
    return CheckForLoop(s, p->baseStyle);
}

void wxStyleList::StyleHasNewChild(wxStyle *s, wxStyle *c)
{
  wxNode *cnode, *snode, *node;

  /* Need to maintain the invariant that parents are in the list
     before children... */
  cnode = Member(c);
  snode = Member(s);
  
  for (node = cnode; node; node = node->Next()) {
    if (PTREQ(node, snode)) {
      /* Move base style to before this style */
      DeleteNode(snode);
      Insert(cnode, s);
      break;
    }
  }
}

int wxStyleList::Number(void)
{
  return wxList::Number();
}

wxStyle *wxStyleList::IndexToStyle(int i)
{
  wxNode *node;

  for (node = First(); i && node; i--, node = node->Next());

  if (node)
    return (wxStyle *)node->Data();
  else
    return NULL;
}
 
int wxStyleList::StyleToIndex(wxStyle *s)
{
  wxNode *node;
  int i = 0;

  for (node = First(); 
       node && PTRNE((wxStyle *)node->Data(), s); 
       i++, node = node->Next());

  if (node)
    return i;
  else
    return -1;
}

void wxStyleList::AdjustUsage(Bool newUser)
{
  if (newUser)
    usage++;
  else
    --usage;
}

Bool wxStyleList::IsUsed(void)
{
  return !!usage;
}

Bool wxStyleList::WriteToFile(class wxMediaStreamOut &f)
{
  return wxmbWriteStylesToFile(this, f);
}

wxStyle *wxStyleList::MapIndexToStyle(int i)
{
  if (styleMap && i < numMappedStyles)
    return styleMap[i];
  else
    return basic;
}

wxStyleList *wxReadStyleList(class wxMediaStreamIn &f)
{
  wxStyleList *l = new wxStyleList;

  return wxmbReadStylesFromFile(l, f, 0);
}

static wxList *readStyles = NULL;

void wxmbSetupStyleReadsWrites(void)
{
  readStyles = new wxList(wxKEY_INTEGER);
}

void wxmbDoneStyleReadsWrites(void)
{
  wxNode *node;
  wxStyleList *l;

  for (node = readStyles->First(); node; node = node->Next()) { 
    l = (wxStyleList *)node->Data();
    if (l->styleMap) {
      delete[] l->styleMap;
      l->styleMap = NULL;
    }
    l->listId = 0;
  }

  delete readStyles;
  readStyles = NULL;
}

static int FamilyStandardToThis(int v)
{
  switch (v) {
  case 71:
    return wxDECORATIVE;
  case 72:
    return wxROMAN;
  case 73:
    return wxSCRIPT;
  case 74:
    return wxSWISS;
  case 75:
    return wxMODERN;
  case 76:
    return wxTELETYPE;
  case 70:
  default:
    return wxDEFAULT;
  }
}

static int FamilyThisToStandard(int v)
{
  switch (v) {
  case wxDECORATIVE:
    return 71;
  case wxROMAN:
    return 72;
  case wxSCRIPT:
    return 73;
  case wxSWISS:
    return 74;
  case wxMODERN:
    return 75;
  case wxTELETYPE:
    return 76;
  case wxDEFAULT:
  default:
    return 70;
  }
}

static int WeightStandardToThis(int v)
{
  switch (v) {
  case 91:
    return wxLIGHT;
  case 92:
    return wxBOLD;
  case 90:
  default:
    return wxNORMAL;
  }
}

static int WeightThisToStandard(int v)
{
  switch (v) {
  case wxLIGHT:
    return 91;
  case wxBOLD:
    return 92;
  case wxNORMAL:
  default:
    return 90;
  }
}

static int StyleStandardToThis(int v)
{
  switch (v) {
  case 93:
    return wxITALIC;
  case 94:
    return wxSLANT;
  case 90:
  default:
    return wxNORMAL;
  }
}

static int StyleThisToStandard(int v)
{
  switch (v) {
  case wxITALIC:
    return 93;
  case wxSLANT:
    return 94;
  case wxNORMAL:
  default:
    return 90;
  }
}

wxStyleList *wxmbReadStylesFromFile(wxStyleList *styleList, 
				    wxMediaStreamIn& f, 
				    Bool overwritename)
{
#define MAX_STYLE_NAME 256
  int baseIndex, shiftIndex;
  long nameSize;
  char name[MAX_STYLE_NAME];
  char face[MAX_STYLE_NAME];
  short r, g, b;
  int i, isJoin, listId;
  wxStyleDelta delta;
  wxNode *node;

  f >> listId;
  
  if ((node = readStyles->Find(listId)))
    return (wxStyleList *)node->Data();

  f >> styleList->numMappedStyles;
  styleList->styleMap = new wxStyle*[styleList->numMappedStyles];

  styleList->styleMap[0] = styleList->BasicStyle();
  for (i = 1; i < styleList->numMappedStyles; i++) {
    f >> baseIndex;

    if (baseIndex >= i) {
      wxMessageBox("Bad style index.", "Error");
      return FALSE;
    }

    nameSize = MAX_STYLE_NAME;
    f.Get((long *)&nameSize, (char *)name);

    f >> isJoin;

    if (isJoin) {
      f >> shiftIndex;

      styleList->styleMap[i] = 
	styleList->FindOrCreateJoinStyle(styleList->styleMap[baseIndex], 
					 styleList->styleMap[shiftIndex]);
    } else {
      f >> delta.family;
      delta.family = FamilyStandardToThis(delta.family);

      nameSize = MAX_STYLE_NAME;
      f.Get((long *)&nameSize, (char *)face);
      
      if (*face)
	delta.face = copystring(face);
      else
	delta.face = NULL;

      // printf("%d %s\n", delta.family, delta.face ? delta.face : "NULL");
      
      f >> delta.sizeMult;
      f >> delta.sizeAdd;
      f >> delta.weightOn;
      delta.weightOn = WeightStandardToThis(delta.weightOn);
      f >> delta.weightOff;
      delta.weightOff = WeightStandardToThis(delta.weightOff);
      f >> delta.styleOn;
      delta.styleOn = StyleStandardToThis(delta.styleOn);
      f >> delta.styleOff;
      delta.styleOff = StyleStandardToThis(delta.styleOff);
      f >> delta.underlinedOn;
      f >> delta.underlinedOff;
      if (WXME_VERSION_ONE() || WXME_VERSION_TWO()) {
	delta.transparentTextBackingOn = FALSE;
	delta.transparentTextBackingOff = FALSE;
      } else {
	f >> delta.transparentTextBackingOn;
	f >> delta.transparentTextBackingOff;
      }
      
      f >> delta.foregroundMult.r;
      f >> delta.foregroundMult.g;
      f >> delta.foregroundMult.b;
      f >> delta.backgroundMult.r;
      f >> delta.backgroundMult.g;
      f >> delta.backgroundMult.b;
      f >> r;
      f >> g;
      f >> b;
      delta.foregroundAdd.Set(r, g, b);
      f >> r;
      f >> g;
      f >> b;
      delta.backgroundAdd.Set(r, g, b);
      if (WXME_VERSION_ONE() || WXME_VERSION_TWO()) {
	if (r || g || b)
	  delta.transparentTextBackingOff = TRUE;
      }

      f >> delta.alignmentOn;
      f >> delta.alignmentOff;

      styleList->styleMap[i] = 
	styleList->FindOrCreateStyle(styleList->styleMap[baseIndex], &delta);
    }

    if (*name)
      styleList->styleMap[i] = 
	(overwritename 
	 ? styleList->ReplaceNamedStyle(name, styleList->styleMap[i])
	 : styleList->NewNamedStyle(name, styleList->styleMap[i]));
  }

  readStyles->Append(listId, styleList);

  return styleList;
}

Bool wxmbWriteStylesToFile(wxStyleList *styleList, wxMediaStreamOut &f)
{
  int i, count;
  wxStyle *style;
  short r, g, b;
  char *name;
  wxStyleDelta delta;

  if (styleList->listId) {
    f << styleList->listId;
    return TRUE;
  }

  styleList->listId = readStyles->Number() + 1;

  f << styleList->listId;

  count = styleList->Number();

  f << count;

  // Basic style is implied

  for (i = 1; i < count; i++) {
    style = styleList->IndexToStyle(i);

    f << styleList->StyleToIndex(style->GetBaseStyle());
    if ((name = style->GetName()))
      f << name;
    else
      f << "";

    if (style->IsJoin()) {
      f << 1;

      f << styleList->StyleToIndex(style->GetShiftStyle());
    } else {
      style->GetDelta(delta);
      
      f << 0;

      f << FamilyThisToStandard(delta.family);
      if (delta.face)
	f << delta.face;
      else
	f << "";

      f << delta.sizeMult;
      f << delta.sizeAdd;
      f << WeightThisToStandard(delta.weightOn); 
      f << WeightThisToStandard(delta.weightOff);
      f << StyleThisToStandard(delta.styleOn);
      f << StyleThisToStandard(delta.styleOff);
      f << delta.underlinedOn;
      f << delta.underlinedOff;
      f << delta.transparentTextBackingOn;
      f << delta.transparentTextBackingOff;

      f << delta.foregroundMult.r;
      f << delta.foregroundMult.g;
      f << delta.foregroundMult.b;
      f << delta.backgroundMult.r;
      f << delta.backgroundMult.g;
      f << delta.backgroundMult.b;
      delta.foregroundAdd.Get(&r, &g, &b);
      f << r;
      f << g;
      f << b;
      delta.backgroundAdd.Get(&r, &g, &b);
      f << r;
      f << g;
      f << b;

      f << delta.alignmentOn;
      f << delta.alignmentOff;
    }
  }

  readStyles->Append(styleList->listId, styleList);

  return TRUE;
}
