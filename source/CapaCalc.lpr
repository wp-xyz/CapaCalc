program CapaCalc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ccMain, ccBaseFrame, ccPlanarCapFrame, ccGlobal, ccStrings,
  ccSphericalCapFrame, ccCylindricalCapFrame, ccPNJunctionCapFrame,
  ccLinePlaneCapFrame, ccCylPlaneCapFrame, cc2CylCapFrame, ccSeriesCapFrame,
  ccManyCapFrame
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  MainForm.BeforeRun;
  Application.Run;
end.

