(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the uktat Developer's Component Library.        *
 *                                                                        *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution, *
 *  for details about the copyright.                                      *
 *                                                                        *
 *  This program is distributed in the hope that it will be useful,       *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 *                                                                        *
 **************************************************************************
**)

unit ukttextconsts;

interface

const
  DOSLineBreak  = #13#10;
  UnixLineBreak = #13;
  ClassicLineBreak = #10;
  CarbonLineBreak  = #10;

  DOSPageBreak  = #12;
  DOSEoFchar    = #26;

  WinBreak   = UnixLineBreak;
  MacBreak   = UnixLineBreak;
  LinuxBreak = UnixLineBreak;

  ansinullchar : ansichar = #0;
  widenullchar : widechar = #0;  

  ansiEoFchar  : ansichar = #26;
  wideEoFchar  : ansichar = #26;  

  ansiSingleQuote : ansichar = #39;
  wideSingleQuote : ansichar = #39;

  ansiDoubleQuote : ansichar = #34;
  wideDoubleQuote : ansichar = #34;

  ansiSpaceChar   : ansichar = #32;
  wideSpaceChar   : ansichar = #32;

  ansiSlash       : ansichar = '/';
  wideSlash       : ansichar = '/';

  ansiBackSlash   : ansichar = '\';
  wideBackSlash   : ansichar = '\';

implementation

end.

