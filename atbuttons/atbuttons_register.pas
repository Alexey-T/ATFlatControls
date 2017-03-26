unit atbuttons_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATButtons, ATButtonsToolbar, ATListbox, ATLinkLabel,
  ATPanelSimple, LResources;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [
    TATButton,
    TATButtonsToolbar,
    TATListbox,
    TATPanelSimple,
    TLinkLabel
    ]);
end;

initialization
  {$I res/icons.lrs}

end.

