unit ATCanvasPrimitives;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

procedure CanvasPaintTriangleDown(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
procedure CanvasPaintLine(C: TCanvas; pnt1, pnt2: TPoint; AColor: TColor);
procedure CanvasPaintXMark(C: TCanvas; const R: TRect; AColor: TColor; AOffset: integer);


implementation

procedure CanvasPaintTriangleDown(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
begin
  C.Brush.Color:= ColorToRGB(AColor);
  C.Pen.Color:= ColorToRGB(AColor);
  C.Polygon([
    Point(ACoord.X - ASize*2, ACoord.Y - ASize),
    Point(ACoord.X + ASize*2, ACoord.Y - ASize),
    Point(ACoord.X, ACoord.Y + ASize)
    ]);
end;

procedure CanvasPaintLine(C: TCanvas; pnt1, pnt2: TPoint; AColor: TColor);
begin
  C.Pen.Color:= ColorToRGB(AColor);
  {$ifdef FPC}
  C.Line(pnt1, pnt2);
  {$else}
  C.MoveTo (pnt1.x, pnt1.y);
  C.LineTo (pnt2.x, pnt2.y);
  {$endif}
end;

procedure CanvasPaintXMark(C: TCanvas; const R: TRect; AColor: TColor; AOffset: integer);
var
  Xm, Ym, X1, Y1, X2, Y2: integer;
begin
  C.Pen.Color:= ColorToRGB(AColor);
  Xm:= (R.Left+R.Right) div 2;
  Ym:= (R.Top+R.Bottom) div 2;
  X1:= R.Left+AOffset;
  X2:= R.Right-AOffset;
  Y1:= Ym-(Xm-X1);
  Y2:= Ym+(X2-Xm);

  C.MoveTo(X1, Y1);
  C.LineTo(X2+1, Y2+1);
  C.MoveTo(X1, Y2);
  C.LineTo(X2+1, Y1-1);

  C.MoveTo(X1+1, Y1);
  C.LineTo(X2+2, Y2+1);
  C.MoveTo(X1+1, Y2);
  C.LineTo(X2+2, Y1-1);
end;


end.

