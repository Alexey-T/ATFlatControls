unit atflatcontrols_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 
  ATButtons, ATFlatToolbar, 
  ATListbox, ATLinkLabel, ATScrollbar, ATStatusbar,
  ATPanelSimple, ATPanelColor, ATGauge,
  LResources;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [
    TATButton,
    TATFlatToolbar,
    TATListbox,
    TATScroll,
    TATStatus,
    TATPanelSimple,
    TATPanelColor,
    TATLabelLink,
    TGauge
    ]);
end;

initialization
  {$I res/icons.lrs}

end.

