{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0
}

unit ATListbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls;

type
  TATListboxDrawItemEvent = procedure(Sender: TObject; AIndex: integer; const ARect: TRect) of object;

type
  { TATListbox }

  TATListbox = class(TCustomControl)
  private
    FOnClick: TNotifyEvent;
    FOnDrawItem: TATListboxDrawItemEvent;
    FItemCount,
    FItemIndex,
    FItemHeight,
    FItemTop,
    FItemBottom: integer;
    procedure DoClick;
    procedure SetItemCount(AValue: integer);
    procedure SetItemIndex(AValue: integer);
    procedure UpdateScrollbar;
  protected
    procedure Paint; override;
  public
    ColorBg: TColor;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ItemCount: integer read FItemCount write SetItemCount;
  published
    property ItemHeight: integer read FItemHeight write FItemHeight;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDrawItem: TATListboxDrawItemEvent read FOnDrawItem write FOnDrawItem;
  end;

implementation

uses
  Math, Types, LCLType, LCLIntf;

{ TATListbox }

procedure TATListbox.UpdateScrollbar;
var
  si: TScrollInfo;
begin
  FillChar(si{%H-}, SizeOf(si), 0);
  si.cbSize:= SizeOf(si);
  si.fMask:= SIF_ALL;
  si.nMin:= 0;
  si.nMax:= FItemCount;
  si.nPage:= ClientHeight div FItemHeight;
  si.nPos:= FItemIndex;
  SetScrollInfo(Handle, SB_VERT, si, True);
end;

procedure TATListbox.Paint;
var
  r: TRect;
  index: integer;
begin
  inherited;

  UpdateScrollbar;

  r:= ClientRect;
  Canvas.Brush.Color:= ColorBg;
  Canvas.FillRect(r);

  for index:= FItemTop to FItemCount-1 do
  begin
    r.Top:= (index-FItemTop)*FItemHeight;
    r.Bottom:= r.Top+FItemHeight;
    r.Left:= 0;
    r.Right:= ClientWidth;
    if r.Top>=ClientHeight then Break;

    if r.Bottom<ClientHeight then
      FItemBottom:= index;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(Self, index, r);
  end;
end;

procedure TATListbox.DoClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TATListbox.SetItemCount(AValue: integer);
begin
  if FItemCount= AValue then Exit;
  FItemCount:= AValue;
  Invalidate;
end;

procedure TATListbox.SetItemIndex(AValue: integer);
begin
  if FItemIndex= AValue then Exit;
  FItemIndex:= AValue;

  if FItemIndex<FItemTop then
    FItemTop:= FItemIndex
  else
  if FItemIndex>FItemBottom then
    FItemTop:= FItemIndex-(ClientHeight div FItemHeight)+1;

  Invalidate;
end;


constructor TATListbox.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle
    +[csOpaque, csNoFocus]
    -[csDoubleClicks, csTripleClicks];

  Width:= 150;
  Height:= 120;

  FOnClick:= nil;
  FOnDrawItem:= nil;

  ColorBg:= clLtGray;
  FItemCount:= 0;
  FItemIndex:= 0;
  FItemHeight:= 25;
  FItemTop:= 0;
end;

destructor TATListbox.Destroy;
begin
  inherited;
end;

initialization

end.

