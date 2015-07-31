unit atbuttons_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATButtons, ATListbox, LResources;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [TATButton, TATListbox]);
end;

initialization
  {$I res/icons.lrs}

end.

