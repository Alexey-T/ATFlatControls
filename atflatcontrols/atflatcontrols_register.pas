unit atflatcontrols_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 
  ATButtons, ATButtonsToolbar, 
  ATListbox, ATLinkLabel, ATScrollbar, ATStatusbar,
  ATPanelSimple, ATPanelColor, ATGauge,
  LResources;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [
    TATButton,
    TATButtonsToolbar,
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

