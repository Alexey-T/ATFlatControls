{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATPanelSimple;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifdef windows}
  Windows, Messages,
  {$endif}
  Classes, SysUtils, Controls, ExtCtrls;

type

  { TATPanelSimple }

  TATPanelSimple = class(TCustomControl)
  private
    FFocusable: boolean;
  public
    constructor Create(AOwner: TComponent); override;
  public
    function CanFocus: boolean; override;
    property Focusable: boolean read FFocusable write FFocusable;
  published
    property Align;
    property Anchors;
    property AutoSize;
    {$ifdef FPC}
    property BorderSpacing;
    {$endif}
    property Color;
    property Enabled;
    property ParentColor;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnResize;
  end;

type

  { TATPanelNoFlicker }

  TATPanelNoFlicker = class(TPanel)
  protected
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$endif}
  end;


implementation

{ TATPanelSimple }

constructor TATPanelSimple.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle + [
    csAcceptsControls {$ifdef FPC},
    csNoFocus{$endif}];

  Width:= 150;
  Height:= 100;
  Caption:= '';
  FFocusable:= false;
end;

function TATPanelSimple.CanFocus: boolean;
begin
  Result:= FFocusable;
end;

{ TATPanelNoFlicker }

{$ifdef windows}
procedure TATPanelNoFlicker.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  R: TRect;
begin
  //to avoid flickering with white on app startup
  if Message.DC<>0 then
  begin
    if ParentColor and Assigned(Parent) then
      Brush.Color:= Parent.Brush.Color
    else
      Brush.Color:= Color;
    R.Left:= 0;
    R.Top:= 0;
    R.Width:= Width;
    R.Height:= Height;
    Windows.FillRect(Message.DC, R, Brush.Reference.Handle);
  end;

  //to remove flickering on resize and mouse-over
  Message.Result:= 1;
end;
{$endif}

end.

