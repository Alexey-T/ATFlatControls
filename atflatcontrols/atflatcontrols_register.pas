unit atflatcontrols_register;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$else}
  {$define windows}
  {$ifdef VER150} //Delphi 7
    {$define WIDE}
  {$endif}
{$endif}

interface

uses
  Classes, SysUtils, 
  ATButtons, ATFlatToolbar, 
  ATListbox, ATLinkLabel, ATScrollbar, ATStatusbar,
  ATPanelSimple, ATPanelColor, ATGauge {$ifndef FPC};{$endif}
  {$ifdef FPC}
  , LResources;
  {$endif}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [
    TATButton,
    TATFlatToolbar,
    TATListbox,
    TATScrollbar,
    TATStatus,
    TATPanelSimple,
    TATPanelColor,
    TATLabelLink,
    TATGauge
    ]);
end;

initialization
  {$ifdef FPC}
  {$I res/icons.lrs}
  {$endif}

end.

