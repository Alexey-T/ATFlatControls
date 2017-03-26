unit atbuttons_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 
  ATButtons, ATButtonsToolbar, 
  ATListbox, ATLinkLabel,
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
    TATPanelSimple,
    TATPanelColor,
    TLinkLabel
    ]);
end;

initialization
  {$I res/icons.lrs}

end.

