{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATPanelColor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls, Types;

type
  { TATPanelColor }

  TATPanelColor = class(TCustomControl)
  private
    FBorderColor: TColor;
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure Paint; override;
    procedure Resize; override;
  published
    property Align;
    property Caption;
    property Color;
    property ParentColor;
    property Enabled;
    property Font;
    property Visible;
    property BorderColor: TColor read FBorderColor write FBorderColor default clBlack;
    property BorderWidth;
    property OnClick;
    property OnDblClick;
    property OnResize;
  end;


implementation

{ TATPanelColor }

constructor TATPanelColor.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= '';
  Color:= clWhite;
  BorderStyle:= bsNone;
  BorderWidth:= 0;
  BorderColor:= clBlack;
end;

procedure TATPanelColor.Paint;
var
  R: TRect;
  Pnt: TPoint;
  Size: TSize;
begin
  //inherited;
  R:= ClientRect;

  Canvas.Brush.Style:= bsSolid;
  Canvas.Brush.Color:= Color;
  Canvas.FillRect(R);

  if BorderWidth>0 then
    Canvas.Frame3d(R, BorderColor, BorderColor, BorderWidth);

  if Caption<>'' then
  begin
    Canvas.Font.Assign(Self.Font);
    Size:= Canvas.TextExtent(Caption);
    Pnt.X:= (R.Right-Size.cx) div 2;
    Pnt.Y:= (R.Bottom-Size.cy) div 2;
    Canvas.TextOut(Pnt.X, Pnt.Y, Caption);
  end;
end;

procedure TATPanelColor.Resize;
begin
  inherited Resize;
  Invalidate;
end;


end.

