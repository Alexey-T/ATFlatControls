unit atbuttons_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 
  ATButtons, ATButtonsToolbar, 
  ATListbox, ATLinkLabel, ATScrollbar,
  ATPanelSimple, ATPanelColor, 
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
    TATPanelSimple,
    TATPanelColor,
    TLinkLabel
    ]);
end;

initialization
  {$I res/icons.lrs}

end.

