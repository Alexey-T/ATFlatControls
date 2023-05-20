{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATFlatThemes;

{$ifdef FPC}
{$ModeSwitch advancedrecords}
{$endif}

interface

uses
  Classes, Graphics;

type
  PATFlatTheme = ^TATFlatTheme;

  TATButtonOverlayPosition = (
    bopLeftTop,
    bopRightTop,
    bopLeftBottom,
    bopRightBottom
    );

  { TATFlatTheme }

  TATFlatTheme = record
    FontName: string;
    FontSize: integer;
    FontQuality: TFontQuality;
    FontStyles: TFontStyles;

    MonoFontName: string;
    MonoFontSize: integer;

    ColorFont: TColor;
    ColorFontDisabled: TColor;
    ColorFontListbox: TColor;
    ColorFontListboxSel: TColor;
    ColorFontListboxHeader: TColor;
    ColorFontOverlay: TColor;
    ColorBgPassive: TColor;
    ColorBgOver: TColor;
    ColorBgChecked: TColor;
    ColorBgDisabled: TColor;
    ColorBgListbox: TColor;
    ColorBgListboxSel: TColor;
    ColorBgListboxHottrack: TColor;
    ColorBgListboxHeader: TColor;
    ColorBgOverlay: TColor;
    ColorListboxBorderPassive: TColor;
    ColorListboxBorderFocused: TColor;
    ColorArrows: TColor;
    ColorArrowsOver: TColor;
    ColorSeparators: TColor;
    ColorBorderPassive: TColor;
    ColorBorderOver: TColor;
    ColorBorderFocused: TColor;
    EnableColorBgOver: boolean;

    ColoredLineWidth: integer;
    MouseoverBorderWidth: integer;
    PressedBorderWidth: integer;
    PressedCaptionShiftY: integer;
    PressedCaptionShiftX: integer;
    BoldBorderWidth: integer;
    ChoiceBorderWidth: integer;
    ArrowSize: integer;
    GapForAutoSize: integer;
    TextOverlayPosition: TATButtonOverlayPosition;
    SeparatorOffset: integer;
    XMarkWidth: integer;
    XMarkOffsetLeft: integer;
    XMarkOffsetRight: integer;
    XMarkLineWidth: integer;
    CrossLineForDisabled: boolean;
    CrossLineWidth: integer;

    ScalePercents: integer;
    ScaleFontPercents: integer;
    function DoScale(AValue: integer): integer;
    function DoScaleFont(AValue: integer): integer;
  end;

var
  ATFlatTheme: TATFlatTheme;

implementation

{ TATFlatTheme }

function TATFlatTheme.DoScale(AValue: integer): integer;
begin
  Result:= AValue * ScalePercents div 100;
end;

function TATFlatTheme.DoScaleFont(AValue: integer): integer;
begin
  if ScaleFontPercents=0 then
    Result:= DoScale(AValue)
  else
    Result:= AValue * ScaleFontPercents div 100;
end;


initialization

  with ATFlatTheme do
  begin
    FontName:= 'default';
    FontSize:= 10;
    FontQuality:= fqDefault;
    FontStyles:= [];

    ColorFont:= $303030;
    ColorFontDisabled:= $808088;
    ColorFontListbox:= ColorFont;
    ColorFontListboxSel:= clWhite;
    ColorFontListboxHeader:= ColorFont;
    ColorFontOverlay:= clWhite;
    ColorBgPassive:= $e0e0e0;
    ColorBgOver:= $90a080;
    ColorBgChecked:= $b0b0b0;
    ColorBgDisabled:= $c0c0d0;
    ColorBgListbox:= ColorBgPassive;
    ColorBgListboxSel:= clMedGray;
    ColorBgListboxHottrack:= clMoneyGreen;
    ColorBgListboxHeader:= $80aa80;
    ColorBgOverlay:= clRed;
    ColorListboxBorderPassive:= $a0a0a0;
    ColorListboxBorderFocused:= clNavy;
    ColorArrows:= clGray;
    ColorArrowsOver:= clBlue;
    ColorSeparators:= clDkGray;
    ColorBorderPassive:= $a0a0a0;
    ColorBorderOver:= $d0d0d0;
    ColorBorderFocused:= clNavy;

    //LCL gives incorrect mouse-hover highlight for buttons,
    //for CudaText Find dialog, on gtk2 and windows too
    EnableColorBgOver:= false;

    ColoredLineWidth:= 4;
    MouseoverBorderWidth:= 1;
    PressedBorderWidth:= 3;
    PressedCaptionShiftX:= 0;
    PressedCaptionShiftY:= 1;
    BoldBorderWidth:= 3;
    ChoiceBorderWidth:= 1;
    ArrowSize:= 2;
    GapForAutoSize:= 8;
    TextOverlayPosition:= bopRightBottom;
    SeparatorOffset:= 2;
    XMarkWidth:= 8;
    XMarkOffsetLeft:= 1;
    XMarkOffsetRight:= 1;
    XMarkLineWidth:= 1;
    CrossLineForDisabled:= true;
    CrossLineWidth:= 2;

    ScalePercents:= 100;
    ScaleFontPercents:= 0;
  end;

end.
