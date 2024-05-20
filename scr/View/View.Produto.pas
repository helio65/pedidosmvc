unit View.Produto;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Mask, Vcl.StdCtrls,
  Vcl.Grids, Vcl.DBGrids, Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TFrmProduto = class(TForm)
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
    Label3: TLabel;
    Label4: TLabel;
    edtCodigo: TEdit;
    edtDescricao: TEdit;
    edtValor: TEdit;
    Panel6: TPanel;
    btnVoltar: TBitBtn;
    Label2: TLabel;
    edtEstoque: TEdit;
    btnImagens: TBitBtn;
    FDMProduto: TFDMemTable;
    dtsProduto: TDataSource;
    FDMProdutoCO_PRODUTO: TIntegerField;
    FDMProdutoTX_DESCRICAO: TStringField;
    FDMProdutoVL_VENDA: TCurrencyField;
    FDMProdutoNU_ESTOQUE: TFloatField;
    FDMProdutoDT_CADASTRO: TDateTimeField;
    FDMProdutoDT_ALTERACAO: TDateTimeField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

initialization
  RegisterClass(TFrmProduto);

Finalization
  unRegisterClass(TFrmProduto);

end.
