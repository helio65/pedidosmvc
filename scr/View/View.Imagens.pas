unit View.Imagens;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons,
  Data.DB, Vcl.Grids, Vcl.DBGrids, Vcl.DBCtrls, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Vcl.ExtDlgs, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg;

type
  TFrmImagem = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    btnNovo: TBitBtn;
    btnGravar: TBitBtn;
    btnCancelar: TBitBtn;
    btnApagar: TBitBtn;
    btnFechar: TBitBtn;
    gridBase: TDBGrid;
    FDMImagens: TFDMemTable;
    dtsImagens: TDataSource;
    FDMImagensCO_IMAGEM: TIntegerField;
    FDMImagensCO_PRODUTO: TIntegerField;
    FDMImagensIM_PRODUTO: TBlobField;
    FDMImagensTX_EXTENSAO: TStringField;
    FDMImagensDT_CADASTRO: TDateTimeField;
    btnEditar: TBitBtn;
    btnVoltar: TBitBtn;
    DBImage1: TDBImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

{ TFrmImagem }

initialization
  RegisterClass(TFrmImagem);

Finalization
  unRegisterClass(TFrmImagem);

end.
