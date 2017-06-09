{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATPanelSimple;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TATPanelSimple }

  TATPanelSimple = class(TCustomControl)
  public
    constructor Create(AOwner: TComponent); override;
  public
    function CanFocus: boolean; override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Color;
    property Enabled;
    property ParentColor;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnResize;
  end;

implementation

{ TATPanelSimple }

constructor TATPanelSimple.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle:= ControlStyle + [
    csAcceptsControls,
    csNoFocus];

  Width:= 180;
  Height:= 100;
  Caption:= '';
end;

function TATPanelSimple.CanFocus: boolean;
begin
  Result:= false;
end;

end.

