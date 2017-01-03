{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATButtonsToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls,
  ImgList, Menus, ATButtons;

type
  { TATButtonsToolbar }

  TATButtonsToolbar = class(TPanel)
  private
    FImages: TImageList;
    FSizeIncToIcon: integer;
    FStringSep: string;
    procedure PopupForDropdownClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddButton(AImageIndex: integer;
      AOnClick: TNotifyEvent;
      const ACaption: string=''; const AHint: string='');
    procedure AddDropdown(
      const ACaption: string;
      AMenu: TPopupMenu; ADropdownEvent: TNotifyEvent=nil);
    procedure AddSep;
    procedure UpdateControls;
    function ButtonCount: integer;
    function GetButton(AIndex: integer): TATButton;
    property Buttons[AIndex: integer]: TATButton read GetButton;
    property StringSeparator: string read FStringSep write FStringSep;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Enabled;
    property Visible;
    property ShowHint;
    property ParentShowHint;
    property Images: TImageList read FImages write FImages;
    property SizeIncrementToIcon: integer read FSizeIncToIcon write FSizeIncToIcon default 6;
  end;

implementation

{ TATButtonsToolbar }

constructor TATButtonsToolbar.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= '';
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  FImages:= nil;
  FSizeIncToIcon:= 6;
  FStringSep:= Utf8Encode(#$25be);
end;

destructor TATButtonsToolbar.Destroy;
begin
  inherited;
end;

procedure TATButtonsToolbar.UpdateControls;
var
  C: TControl;
  i: integer;
begin
  if not Assigned(FImages) then exit;
  if ControlCount=0 then exit;

  Height:= FImages.Height+FSizeIncToIcon;

  //update control sizes
  //width only for buttons
  for i:= ControlCount-1 downto 0 do
  begin
    C:= Controls[i];
    C.Height:= FImages.Height+FSizeIncToIcon;
    if C is TATButton then
      if (C as TATButton).ImageIndex>=0 then
        C.Width:= FImages.Width+FSizeIncToIcon;
  end;

  //place controls left to right
  Controls[0].Left:= 0;
  Controls[0].Top:= 0;
  for i:= ControlCount-1 downto 1 do
    Controls[i].AnchorToNeighbour(akLeft, 0, Controls[i-1]);

  //paint
  for i:= 0 to ControlCount-1 do
    Controls[i].Invalidate;
end;

function TATButtonsToolbar.ButtonCount: integer;
begin
  Result:= ControlCount;
end;

function TATButtonsToolbar.GetButton(AIndex: integer): TATButton;
begin
  Result:= nil;
  if (AIndex>=0) and (AIndex<ControlCount) then
    if Controls[AIndex] is TATButton then
      Result:= Controls[AIndex] as TATButton;
end;

procedure TATButtonsToolbar.AddButton(AImageIndex: integer;
  AOnClick: TNotifyEvent; const ACaption: string; const AHint: string);
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;
  b.Flat:= true;
  b.Caption:= ACaption;
  b.Hint:= AHint;
  b.Images:= FImages;
  b.ImageIndex:= AImageIndex;
  b.ShowCaption:= false;
  b.ShowHint:= true;
  b.OnClick:= AOnClick;
end;

procedure TATButtonsToolbar.AddDropdown(
  const ACaption: string;
  AMenu: TPopupMenu; ADropdownEvent: TNotifyEvent = nil);
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;

  b.Width:= cATButtonArrowSize+2*cATButtonArrowHorzIndent;
  if ACaption<>'' then
  begin
    b.Canvas.Font.Name:= ATButtonTheme.FontName;
    b.Canvas.Font.Size:= ATButtonTheme.FontSize;
    b.Canvas.Font.Style:= ATButtonTheme.FontStyles;
    b.Width:= b.Width+cATButtonArrowHorzIndent+b.Canvas.TextWidth(ACaption);
  end;

  b.Caption:= ACaption;
  b.Flat:= true;
  b.SpecKind:= abkDropdown;
  b.PopupMenu:= AMenu;
  if ADropdownEvent=nil then
    b.OnClick:= @PopupForDropdownClick
  else
    b.OnClick:= ADropdownEvent;
end;

procedure TATButtonsToolbar.AddSep;
var
  b: TATButton;
begin
  b:= TATButton.Create(Self);
  b.Parent:= Self;
  b.Caption:= '';
  b.Flat:= true;
  b.SpecKind:= abkVerticalLine;
  b.Enabled:= false;
  b.Width:= cATButtonArrowSize+2*cATButtonArrowHorzIndent;
end;

procedure TATButtonsToolbar.PopupForDropdownClick(Sender: TObject);
var
  C: TControl;
  P: TPoint;
begin
  C:= Sender as TControl;
  P:= C.ClientToScreen(Point(0, C.Height));
  C.PopupMenu.PopUp(P.X, P.Y);
end;


end.

