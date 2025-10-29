program CapaCalc;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  {$IFDEF UseCThreads}cthreads,{$ENDIF}
  clocale,
 {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ccMain, ccBaseFrame, ccPlanarCapFrame, ccGlobal, ccStrings,
  ccSphericalCapFrame, ccCylindricalCapFrame, ccPNJunctionCapFrame,
  ccLinePlaneCapFrame, ccCylPlaneCapFrame, cc2CylCapFrame, ccSeriesCapFrame,
  ccManyCapFrame
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

