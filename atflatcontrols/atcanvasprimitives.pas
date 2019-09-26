unit ATCanvasPrimitives;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math;

procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
procedure CanvasLine(C: TCanvas; P1, P2: TPoint; AColor: TColor);
procedure CanvasDottedVertLine_Alt(C: TCanvas; Color: TColor; X1, Y1, Y2: integer); inline;
procedure CanvasDottedHorzVertLine(C: TCanvas; Color: TColor; X1, Y1, X2, Y2: integer);
procedure CanvasWavyHorzLine(C: TCanvas; Color: TColor; X1, Y1, X2, Y2: integer; AtDown: boolean);
procedure CanvasRoundedLine(C: TCanvas; Color: TColor; X1, Y1, X2, Y2: integer; AtDown: boolean);

procedure CanvasPaintTriangleUp(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;
procedure CanvasPaintTriangleDown(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;
procedure CanvasPaintTriangleRight(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;

procedure CanvasArrowHorz(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  AArrowLen: integer; //OptUnprintedTabCharLength*ACharSizeX
  AToRight: boolean;
  APointerScale: integer); //OptUnprintedTabPointerScale

procedure CanvasArrowDown(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  APointerLen: integer; //OptUnprintedEndArrowLength
  APointerScale: integer); //OptUnprintedTabPointerScale

procedure CanvasPaintPlusMinus(C: TCanvas; AColorBorder, AColorBG: TColor; ACenter: TPoint; ASize: integer; APlus: boolean); inline;
procedure CanvasPaintXMark(C: TCanvas; const R: TRect; AColor: TColor; AOffset: integer);


implementation

var
  _Pen: TPen = nil;


procedure CanvasLine(C: TCanvas; P1, P2: TPoint; AColor: TColor);
begin
  C.Pen.Color:= ColorToRGB(AColor);
  {$ifdef FPC}
  C.Line(P1, P2);
  {$else}
  C.MoveTo(P1.x, P1.y);
  C.LineTo(P2.x, P2.y);
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


{$ifdef invert_pixels}
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  i, j: integer;
begin
  for j:= R.Top to R.Bottom-1 do
    for i:= R.Left to R.Right-1 do
      C.Pixels[i, j]:= C.Pixels[i, j] xor (not AColor and $ffffff);
end;
{$else}
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  X: integer;
  AM: TAntialiasingMode;
begin
  AM:= C.AntialiasingMode;
  _Pen.Assign(C.Pen);

  X:= (R.Left+R.Right) div 2;
  C.Pen.Mode:= {$ifdef darwin} pmNot {$else} pmNotXor {$endif};
  C.Pen.Style:= psSolid;
  C.Pen.Color:= AColor;
  C.AntialiasingMode:= amOff;
  C.Pen.EndCap:= pecFlat;
  C.Pen.Width:= R.Width;

  C.MoveTo(X, R.Top);
  C.LineTo(X, R.Bottom);

  C.Pen.Assign(_Pen);
  C.AntialiasingMode:= AM;
  C.Rectangle(0, 0, 0, 0); //apply pen
end;
{$endif}

procedure CanvasDottedHorzVertLine(C: TCanvas; Color: TColor; X1, Y1, X2, Y2: integer);
var
  i: integer;
  vis: boolean;
begin
  vis:= false;
  if Y1=Y2 then
  begin
    for i:= X1 to X2 do
    begin
      vis:= not vis;
      if vis then
        C.Pixels[i, Y2]:= Color;
    end;
  end
  else
  begin
    for i:= Y1 to Y2 do
    begin
      vis:= not vis;
      if vis then
        C.Pixels[X1, i]:= Color;
    end;
  end;
end;

procedure CanvasDottedVertLine_Alt(C: TCanvas; Color: TColor; X1, Y1, Y2: integer); inline;
var
  j: integer;
begin
  for j:= Y1 to Y2 do
    if Odd(j) then
      C.Pixels[X1, j]:= Color;
end;

procedure CanvasPaintTriangleUp(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X - ASize*2, ACoord.Y + ASize),
    Point(ACoord.X + ASize*2, ACoord.Y + ASize),
    Point(ACoord.X, ACoord.Y - ASize)
    ]);
end;

procedure CanvasPaintTriangleDown(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X - ASize*2, ACoord.Y - ASize),
    Point(ACoord.X + ASize*2, ACoord.Y - ASize),
    Point(ACoord.X, ACoord.Y + ASize)
    ]);
end;

procedure CanvasPaintTriangleRight(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X - ASize, ACoord.Y - ASize*2),
    Point(ACoord.X + ASize, ACoord.Y),
    Point(ACoord.X - ASize, ACoord.Y + ASize*2)
    ]);
end;


procedure CanvasArrowHorz(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  AArrowLen: integer; //OptUnprintedTabCharLength*ACharSizeX
  AToRight: boolean;
  APointerScale: integer); //OptUnprintedTabPointerScale
const
  cIndent = 1; //offset left/rt
var
  XLeft, XRight, X1, X2, Y, Dx: integer;
begin
  XLeft:= ARect.Left+cIndent;
  XRight:= ARect.Right-cIndent;

  if AArrowLen=0 then
  begin;
    X1:= XLeft;
    X2:= XRight;
  end
  else
  begin
    X1:= XLeft;
    X2:= Min(XRight, X1+AArrowLen);
  end;

  Y:= (ARect.Top+ARect.Bottom) div 2;
  Dx:= ARect.Height * APointerScale div 100;
  C.Pen.Color:= AColorFont;

  C.Line(X1, Y, X2, Y);
  if AToRight then
  begin
    C.MoveTo(X2, Y);
    C.LineTo(X2-Dx, Y-Dx);
    C.MoveTo(X2, Y);
    C.LineTo(X2-Dx, Y+Dx);
  end
  else
  begin
    C.MoveTo(X1, Y);
    C.LineTo(X1+Dx, Y-Dx);
    C.MoveTo(X1, Y);
    C.LineTo(X1+Dx, Y+Dx);
  end;
end;

procedure CanvasArrowDown(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  APointerLen: integer; //OptUnprintedEndArrowLength
  APointerScale: integer); //OptUnprintedTabPointerScale
var
  Len, X, Y1, Y2, Dx: integer;
begin
  X:= (ARect.Left+ARect.Right) div 2;
  Len:= ARect.Height * APointerLen div 100;
  Dx:= ARect.Height * APointerScale div 100;
  C.Pen.Color:= AColorFont;

  Y1:= (ARect.Bottom+ARect.Top-Len) div 2;
  Y2:= Y1+Len;

  C.MoveTo(X, Y1);
  C.LineTo(X, Y2);
  C.MoveTo(X, Y2);
  C.LineTo(X-Dx, Y2-Dx);
  C.MoveTo(X, Y2);
  C.LineTo(X+Dx, Y2-Dx);
end;


procedure CanvasPaintPlusMinus(C: TCanvas; AColorBorder, AColorBG: TColor;
  ACenter: TPoint; ASize: integer; APlus: boolean); inline;
begin
  C.Brush.Color:= AColorBG;
  C.Pen.Color:= AColorBorder;
  C.Rectangle(ACenter.X-ASize, ACenter.Y-ASize, ACenter.X+ASize+1, ACenter.Y+ASize+1);
  C.Line(ACenter.X-ASize+2, ACenter.Y, ACenter.X+ASize-1, ACenter.Y);
  if APlus then
    C.Line(ACenter.X, ACenter.Y-ASize+2, ACenter.X, ACenter.Y+ASize-1);
end;

procedure CanvasWavyHorzLine(C: TCanvas; Color: TColor; X1, Y1, X2, Y2: integer; AtDown: boolean);
const
  cWavePeriod = 2;
  cWaveInc: array[0..cWavePeriod-1] of integer = (0, 2);
var
  Points: array of TPoint;
  PointCount, PointIndex: integer;
  X, Y, NSign: integer;
begin
  PointCount:= (X2-X1+1) div 2;
  if PointCount<3 then exit;
  SetLength(Points, PointCount);

  if AtDown then NSign:= -1 else NSign:= 1;
  PointIndex:= 0;

  for X:= X1 to X2 do
    if not Odd(X) then
    begin
      if PointIndex>=PointCount then Break;
      Y:= Y2 + NSign * cWaveInc[(X-X1) div 2 mod cWavePeriod];
      Points[PointIndex]:= Point(X, Y);
      Inc(PointIndex);
    end;

  if PointIndex+1<PointCount then
    SetLength(Points, PointIndex+1);

  C.Pen.Color:= Color;
  C.Polyline(Points);
end;

procedure CanvasRoundedLine(C: TCanvas; Color: TColor; X1, Y1, X2, Y2: integer; AtDown: boolean);
var
  Points: array[0..3] of TPoint;
begin
  C.Pen.Color:= Color;
  if Y1=Y2 then
  begin
    //paint polyline, 4 points, horz line and 2 edges
    Points[1]:= Point(X1+2, Y1);
    Points[2]:= Point(X2-2, Y2);
    if AtDown then
    begin
      Points[0]:= Point(X1, Y1-2);
      Points[3]:= Point(X2+1, Y2-3);
    end
    else
    begin
      Points[0]:= Point(X1, Y1+2);
      Points[3]:= Point(X2+1, Y2+3);
    end;
    C.Polyline(Points);
  end
  else
  begin
    C.Line(X1, Y1+2, X2, Y2-1);
    //don't draw pixels, other lines did it
  end;
end;


initialization
  _Pen:= TPen.Create;

finalization
  if Assigned(_Pen) then
    FreeAndNil(_Pen);

end.

