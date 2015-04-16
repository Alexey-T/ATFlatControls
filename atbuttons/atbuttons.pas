{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0
}

unit ATButtons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls;

type
  TATButtonTheme = record
    FontName: string;
    FontSize: integer;
    FontStyles: TFontStyles;
    ColorFont,
    ColorBgPassive,
    ColorBgOver,
    ColorBgChecked,
    ColorBorderPassive,
    ColorBorderOver,
    ColorBorderFocused: TColor;
    MouseoverBorderWidth: integer;
    PressedBorderWidth: integer;
    PressedCaptionShiftY: integer;
    PressedCaptionShiftX: integer;
  end;

var
  ATButtonTheme: TATButtonTheme;

type
  { TATSimpleButton }

  TATSimpleButton = class(TCustomControl)
  private
    FPressed,
    FOver,
    FChecked,
    FCheckable,
    FFocusable: boolean;
    FCaption: string;
    FBitmap: TBitmap;
    FOnClick: TNotifyEvent;
    procedure DoClick;
    function IsPressed: boolean;
    procedure SetCaption(AValue: string);
    procedure SetChecked(AValue: boolean);
    procedure SetFocusable(AValue: boolean);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseEnter; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var Key: char); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Caption: string read FCaption write SetCaption;
    property Bitmap: TBitmap read FBitmap write FBitmap;
    property Checked: boolean read FChecked write SetChecked;
    property Checkable: boolean read FCheckable write FCheckable;
    property Focusable: boolean read FFocusable write SetFocusable;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

uses Math, Types;

{ TATSimpleButton }

procedure TATSimpleButton.SetChecked(AValue: boolean);
begin
  if FChecked= AValue then Exit;
  FChecked:= AValue;
  Invalidate;
end;

procedure TATSimpleButton.SetFocusable(AValue: boolean);
begin
  if FFocusable= AValue then Exit;
  FFocusable:= AValue;
  TabStop:= AValue;
end;

procedure TATSimpleButton.SetCaption(AValue: string);
begin
  if FCaption= AValue then Exit;
  FCaption:= AValue;
  Invalidate;
end;

function TATSimpleButton.IsPressed: boolean;
begin
  Result:= FPressed and FOver;
end;

procedure TATSimpleButton.Paint;
var
  r: TRect;
  p: TPoint;
  size, i: integer;
begin
  inherited;

  //----draw bg
  r:= ClientRect;
  Canvas.Brush.Color:=
    IfThen(FChecked, ATButtonTheme.ColorBgChecked,
      IfThen(FOver, ATButtonTheme.ColorBgOver, ATButtonTheme.ColorBgPassive));
  Canvas.FillRect(r);

  //----draw border
  Canvas.Brush.Style:= bsClear;

  Canvas.Pen.Color:=
    IfThen(FOver, ATButtonTheme.ColorBorderOver,
      IfThen(Focused, ATButtonTheme.ColorBorderFocused, ATButtonTheme.ColorBorderPassive));
  Canvas.Rectangle(r);

  size:= 1;
  if IsPressed then size:= ATButtonTheme.PressedBorderWidth else
  if FOver then size:= ATButtonTheme.MouseoverBorderWidth;

  for i:= 1 to size-1 do
  begin
    InflateRect(r, -1, -1);
    Canvas.Rectangle(r);
  end;

  Canvas.Brush.Style:= bsSolid;

  //----draw caption
  if FCaption<>'' then
  begin
    Canvas.Font.Name:= ATButtonTheme.FontName;
    Canvas.Font.Color:= ATButtonTheme.ColorFont;
    Canvas.Font.Size:= ATButtonTheme.FontSize;
    Canvas.Font.Style:= ATButtonTheme.FontStyles;

    p.x:= (ClientWidth - Canvas.TextWidth(FCaption)) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
    p.y:= (ClientHeight - Canvas.TextHeight(FCaption)) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
    Canvas.TextOut(p.x, p.y, FCaption);
  end;

  //----draw bitmap
  if Assigned(FBitmap) then
  begin
    p.x:= (ClientWidth-FBitmap.Width) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftX);
    p.y:= (ClientHeight-FBitmap.Height) div 2 +
      IfThen(IsPressed, ATButtonTheme.PressedCaptionShiftY);
    Canvas.Draw(p.x, p.y, FBitmap);
  end;
end;

procedure TATSimpleButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  bOver: boolean;
begin
  inherited;

  bOver:= PtInRect(ClientRect, Point(X, Y));
  if bOver<>FOver then
  begin
    FOver:= bOver;
    Invalidate;
  end;
end;

procedure TATSimpleButton.MouseLeave;
begin
  inherited;
  FOver:= false;
  Invalidate;
end;

procedure TATSimpleButton.MouseEnter;
begin
  inherited;
  FOver:= true;
  Invalidate;
end;

procedure TATSimpleButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Shift=[ssLeft] then
  begin
    FPressed:= true;
    if FFocusable then
      SetFocus;
  end;

  Invalidate;
end;

procedure TATSimpleButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if IsPressed then
    DoClick;

  FPressed:= false;
  Invalidate;
end;

procedure TATSimpleButton.DoClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
  if FCheckable then
    FChecked:= not FChecked;
  Invalidate;
end;


procedure TATSimpleButton.KeyPress(var Key: char);
begin
  inherited;
  if (Key=' ') then
    DoClick;
end;

procedure TATSimpleButton.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TATSimpleButton.DoExit;
begin
  inherited;
  Invalidate;
end;

constructor TATSimpleButton.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle
    +[csOpaque]
    -[csDoubleClicks, csTripleClicks];

  TabStop:= true;
  Width:= 100;
  Height:= 25;

  FCaption:= 'Button';
  FBitmap:= nil;
  FPressed:= false;
  FOver:= false;
  FChecked:= false;
  FCheckable:= false;
  FFocusable:= true;
  FOnClick:= nil;
end;

destructor TATSimpleButton.Destroy;
begin
  if Assigned(FBitmap) then
    FreeAndNil(FBitmap);

  inherited;
end;

initialization

  with ATButtonTheme do
  begin
    FontName:= 'default';
    FontSize:= 10;
    FontStyles:= [];
    ColorFont:= $303030;
    ColorBgPassive:= $e0e0e0;
    ColorBgOver:= $e0e0e0;
    ColorBgChecked:= $b0b0b0;
    ColorBorderPassive:= $a0a0a0;
    ColorBorderOver:= $d0d0d0;
    ColorBorderFocused:= clNavy;
    MouseoverBorderWidth:= 1;
    PressedBorderWidth:= 3;
    PressedCaptionShiftX:= 0;
    PressedCaptionShiftY:= 1;
  end;

end.

