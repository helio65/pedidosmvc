unit View.Pedido;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Data.DB, Vcl.Grids, Vcl.DBGrids, Vcl.DBCtrls, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Stan.Async, FireDAC.DApt;

type
  TFrmPedido = class(TForm)
    Panel1: TPanel;
    btnNovo: TBitBtn;
    btnEditar: TBitBtn;
    btnGravar: TBitBtn;
    btnCancelar: TBitBtn;
    btnApagar: TBitBtn;
    btnFechar: TBitBtn;
    Panel4: TPanel;
    Panel2: TPanel;
    gridBase: TDBGrid;
    pnlPesquisa: TPanel;
    rdgPesquisa: TRadioGroup;
    pnlControlePesquisa: TPanel;
    comboPesqStatus: TComboBox;
    edtNumero: TEdit;
    Label1: TLabel;
    edtCodigo: TEdit;
    Label2: TLabel;
    FDMPedido: TFDMemTable;
    dtsPedido: TDataSource;
    comboStatus: TComboBox;
    Label3: TLabel;
    FDMPedidoNU_PEDIDO: TIntegerField;
    FDMPedidoCO_EMPRESA: TIntegerField;
    FDMPedidoIN_SITUACAO: TStringField;
    FDMPedidoDT_CADASTRO: TDateTimeField;
    FDMPedidoDT_ALTERACAO: TDateTimeField;
    Label4: TLabel;
    edtValorPedido: TEdit;
    FDMPedidoVL_PEDIDO: TCurrencyField;
    comboCliente: TDBLookupComboBox;
    FDQClientes: TFDQuery;
    dtsClientes: TDataSource;
    btnVoltar: TBitBtn;
    btnProdutos: TBitBtn;
    comboPesqCliente: TDBLookupComboBox;
    FDQClientesPesq: TFDQuery;
    dtsClientesPesq: TDataSource;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

initialization
  RegisterClass(TFrmPedido);

Finalization
  unRegisterClass(TFrmPedido);

end.
