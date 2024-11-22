unit ccSeriesCapFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ccManyCapFrame;

type
  TSeriesCapFrame = class(TManyCapFrame)
  private

  protected

  public
    constructor Create(AOwner: TComponent); override;
    procedure Calculate; override;

  end;

implementation

{$R *.lfm}

constructor TSeriesCapFrame.Create(AOwner: TComponent);
begin
  inherited;
  FIniKey := 'Series';
end;

procedure TSeriesCapFrame.Calculate;
var
  capa, c : double;
  i: Integer;
begin
  try
    capa := 0.0;
    for i:=1 to Length(FEditArray) do
      if (FEditArray[i].Text <> '') and TryStrToFloat(FEditArray[i].Text, c) then
        capa := capa + 1.0 / c;
    if capa <> 0.0 then
      TxtCapa.Text := Format('%.*f', [FDecimals, 1.0 / capa]) else
      TxtCapa.Clear;
  except
    TxtCapa.Clear;
  end;
end;

end.

