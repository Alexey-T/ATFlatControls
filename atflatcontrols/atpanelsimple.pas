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
    FCompositedEnabled: boolean;
    procedure EnableWinStyleComposited;
    procedure Paint; override;
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
procedure TATPanelNoFlicker.Paint;
begin
  inherited Paint;

  //fix CudaText issue #6267: this panel is the side panel title
  //(TAppPanelHost.PanelTitle in CudaText), custom-drawn, not double-buffered
  //on win32. It flashes once when the auto-completion popup is shown for the
  //first time: the popup's first SetWindowPos/show invalidates sibling
  //windows, and the title's repaint shows a separate erase pass (filled
  //with Parent color, light in dark themes) + paint pass = visible flash.
  //WS_EX_COMPOSITED makes Windows paint the control through an off-screen
  //buffer, so the erase pass is never visible. Enabled lazily after the
  //first correct Paint (same as TATButton/TATScrollbar/TATStatus, faa0529):
  //a redirected surface allocated before the first WM_PAINT is white,
  //which flashed white on startup in dark theme (that regression was the
  //reason CreateParams was reverted upstream).
  EnableWinStyleComposited;
end;

procedure TATPanelNoFlicker.EnableWinStyleComposited;
begin
  if FCompositedEnabled then exit;
  FCompositedEnabled:= true;
  if HandleAllocated then
    SetWindowLongPtrW(Handle, GWL_EXSTYLE,
      GetWindowLongPtrW(Handle, GWL_EXSTYLE) or WS_EX_COMPOSITED);
end;

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

