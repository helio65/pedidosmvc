unit View.Usuario;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Data.DB, Vcl.Mask, Vcl.Grids, Vcl.DBGrids, Vcl.ComCtrls, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type
  TFrmUsuario = class(TForm)
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
    Panel4: TPanel;
    Panel5: TPanel;
    gridBase: TDBGrid;
    tabCadastro: TTabSheet;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtCodigo: TEdit;
    edtNomeUsuario: TEdit;
    edtSenha: TEdit;
    Panel6: TPanel;
    btnVoltar: TBitBtn;
    edtLogin: TEdit;
    rdgSituacao: TRadioGroup;
    dtsUsuario: TDataSource;
    FDMUsuario: TFDMemTable;
    FDMUsuarioCO_USUARIO: TIntegerField;
    FDMUsuarioNM_USUARIO: TStringField;
    FDMUsuarioTX_LOGIN: TStringField;
    FDMUsuarioTX_SENHA: TStringField;
    FDMUsuarioIN_ATIVO: TStringField;
    FDMUsuarioDT_CADASTRO: TDateTimeField;
    FDMUsuarioDT_ALTERACAO: TDateTimeField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

initialization
  RegisterClass(TFrmUsuario);

Finalization
  unRegisterClass(TFrmUsuario);

end.
