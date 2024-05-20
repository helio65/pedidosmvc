unit View.Empresa;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.Mask, Vcl.ComCtrls, Data.DB, Vcl.Grids, Vcl.DBGrids, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type
  TFrmEmpresa = class(TForm)
    Panel1: TPanel;
    btnNovo: TBitBtn;
    btnEditar: TBitBtn;
    btnGravar: TBitBtn;
    btnCancelar: TBitBtn;
    btnApagar: TBitBtn;
    btnFechar: TBitBtn;
    Panel2: TPanel;
    pgcPrincipal: TPageControl;
    tabConsulta: TTabSheet;
    tabCadastro: TTabSheet;
    Panel3: TPanel;
    edtCodigo: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtCNPJ: TMaskEdit;
    Label3: TLabel;
    edtRazaoSocial: TEdit;
    Label4: TLabel;
    edtEmail: TEdit;
    Panel4: TPanel;
    Panel5: TPanel;
    gridBase: TDBGrid;
    FDMEmpresa: TFDMemTable;
    dtsEmpresa: TDataSource;
    FDMEmpresaCO_EMPRESA: TIntegerField;
    FDMEmpresaNM_RAZAO_SOCIAL: TStringField;
    FDMEmpresaNU_CNPJ: TStringField;
    FDMEmpresaTX_EMAIL: TStringField;
    FDMEmpresaDT_CADASTRO: TDateTimeField;
    FDMEmpresaDT_ALTERACAO: TDateTimeField;
    Panel6: TPanel;
    btnVoltar: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

initialization
  RegisterClass(TFrmEmpresa);

Finalization
  unRegisterClass(TFrmEmpresa);

end.
