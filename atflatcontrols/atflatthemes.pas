unit ATFlatThemes;

{$ModeSwitch advancedrecords}

interface

uses
  Classes, Graphics;

type
  PATButtonTheme = ^TATButtonTheme;

  { TATButtonTheme }

  TATButtonTheme = record
    FontName: string;
    FontSize: integer;
    FontStyles: TFontStyles;
    ColorFont,
    ColorFontDisabled,
    ColorBgPassive,
    ColorBgOver,
    ColorBgChecked,
    ColorBgDisabled,
    ColorArrows,
    ColorSeparators,
    ColorBorderPassive,
    ColorBorderOver,
    ColorBorderFocused: TColor;
    MouseoverBorderWidth: integer;
    PressedBorderWidth: integer;
    PressedCaptionShiftY: integer;
    PressedCaptionShiftX: integer;
    BoldBorderWidth: integer;
    ChoiceBorderWidth: integer;
    ArrowSize: integer;
    GapForAutoSize: integer;
    ScalePercents: integer;
    ScaleFontPercents: integer;
    function DoScale(AValue: integer): integer;
    function DoScaleFont(AValue: integer): integer;
  end;

var
  ATButtonTheme: TATButtonTheme;

implementation

{ TATButtonTheme }

function TATButtonTheme.DoScale(AValue: integer): integer;
begin
  Result:= AValue * ScalePercents div 100;
end;

function TATButtonTheme.DoScaleFont(AValue: integer): integer;
begin
  if ScaleFontPercents=0 then
    Result:= DoScale(AValue)
  else
    Result:= AValue * ScaleFontPercents div 100;
end;


initialization

  with ATButtonTheme do
  begin
    FontName:= 'default';
    FontSize:= 10;
    FontStyles:= [];
    ColorFont:= $303030;
    ColorFontDisabled:= $808088;
    ColorBgPassive:= $e0e0e0;
    ColorBgOver:= $e0e0e0;
    ColorBgChecked:= $b0b0b0;
    ColorBgDisabled:= $c0c0d0;
    ColorArrows:= clGray;
    ColorSeparators:= clDkGray;
    ColorBorderPassive:= $a0a0a0;
    ColorBorderOver:= $d0d0d0;
    ColorBorderFocused:= clNavy;
    MouseoverBorderWidth:= 1;
    PressedBorderWidth:= 3;
    PressedCaptionShiftX:= 0;
    PressedCaptionShiftY:= 1;
    BoldBorderWidth:= 3;
    ChoiceBorderWidth:= 1;
    ArrowSize:= 2;
    GapForAutoSize:= 8;
    ScalePercents:= 100;
    ScaleFontPercents:= 0;
  end;

end.
