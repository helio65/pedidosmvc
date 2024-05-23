unit Controller.Imagens;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  System.Variants, Vcl.ExtCtrls, Vcl.ExtDlgs, Vcl.Dialogs, Vcl.Graphics,
  System.StrUtils, System.Generics.Collections, FireDAC.Comp.Client,
  FireDAC.Stan.Param, Data.DB, Vcl.Imaging.jpeg, Dao.Conexao, Framework.Factory,
  View.Imagens, Model.Imagens, Dao.Imagens;

type
  TImagensController = class
  private
    FView : TFrmImagem;
    FFactory : TFactory;
    FStatus : TStatus;
    FCo_Produto : Integer;
    FImagensModel : TImagensModel;
    FImagensDao : TImagensDao;
    FOpenPictureDialog : TOpenPictureDialog;
    procedure btnFecharClick(Sender : TObject);
    procedure btnNovoClick(Sender : TObject);
    procedure FormShow(Sender: TObject);
    procedure btnGravarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnApagarClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  public
    constructor Create(co_produto : Integer);
    destructor Destroy; override;
  end;

implementation

{ TImagensController }

procedure TImagensController.btnApagarClick(Sender: TObject);
begin
  if Application.MessageBox('Confirma a exclusão desta Imagem?', 'Atenção', MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = IDYES then
  begin
    Self.FStatus := TStatus.dsDelete;
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    if Self.FImagensDao.Delete(Self.FView.FDMImagensCO_IMAGEM.AsInteger) then
      Self.FormShow(Sender);
  end;
end;

procedure TImagensController.btnCancelarClick(Sender: TObject);
begin
  Self.FFactory.ClearControls(Self.FView);
  Self.FStatus                       := TStatus.dsBrowse;
  Self.FFactory.EnableDisableControls(Self.FView, aDisable);
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
end;

procedure TImagensController.btnFecharClick(Sender: TObject);
begin
  Self.FView.Close;
end;

procedure TImagensController.btnGravarClick(Sender: TObject);
var
  LStream : TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    TBlobField(Self.FView.FDMImagensIM_PRODUTO).SaveToStream(LStream);
    LStream.Position := 0;
    Self.FImagensModel.co_imagem   := Self.FView.FDMImagensCO_IMAGEM.AsInteger;
    Self.FImagensModel.co_produto  := Self.FView.FDMImagensCO_PRODUTO.AsInteger;
    Self.FImagensModel.im_produto  := LStream;
    Self.FImagensModel.tx_extensao := Self.FView.FDMImagensTX_EXTENSAO.AsString;
    if Self.FImagensDao.Insert(Self.FImagensModel) then
    begin

    end;
    Self.FStatus := TStatus.dsBrowse;
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TImagensController.btnNovoClick(Sender: TObject);
var
  LStream : TMemoryStream;
  LJpg : TJpegImage;
begin
  Self.FStatus := TStatus.dsInsert;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  if Self.FOpenPictureDialog.Execute then
  begin
    if not String(Self.FOpenPictureDialog.FileName).IsEmpty then
    begin
      LStream := TMemoryStream.Create;
      LJpg    := TJpegImage.Create;
      try
        if not Self.FView.FDMImagens.Active then
          Self.FView.FDMImagens.Open;

        Self.FView.FDMImagens.Insert;
        Self.FView.FDMImagensCO_IMAGEM.AsInteger := Self.FFactory.GetNextID('INC_CO_IMAGEM', DMConexao.Conexao);
        Self.FView.FDMImagensCO_PRODUTO.AsInteger := Self.FCo_Produto;

        LJpg.LoadFromFile(Self.FOpenPictureDialog.FileName);
        LJpg.SaveToStream(LStream);
        LStream.Position := 0;
        Self.FView.FDMImagensIM_PRODUTO.LoadFromStream(LStream);
        Self.FView.FDMImagensTX_EXTENSAO.AsString := ExtractFileExt(Self.FOpenPictureDialog.FileName).Replace('.', '').Trim;
        Self.FView.FDMImagens.Post;
      finally
        FreeAndNil(LJpg);
        FreeAndNil(LStream);
      end;
    end;
  end;
end;

constructor TImagensController.Create(co_produto : Integer);
begin
  inherited Create;
  Self.FView                     := TFrmImagem.Create(nil);
  Self.FView.KeyPreview          := True;
  Self.FView.btnFechar.OnClick   := Self.btnFecharClick;
  Self.FView.btnNovo.OnClick     := Self.btnNovoClick;
  Self.FView.btnGravar.OnClick   := Self.btnGravarClick;
  Self.FView.OnShow              := Self.FormShow;
  Self.FView.OnKeyPress          := Self.FormKeyPress;
  Self.FView.btnCancelar.OnClick := Self.btnCancelarClick;
  Self.FView.btnApagar.OnClick   := Self.btnApagarClick;
  Self.FFactory                  := TFactory.Create;
  Self.FImagensModel             := TImagensModel.Create;
  Self.FImagensDao               := TImagensDao.Create;
  Self.FCo_Produto               := co_produto;
  Self.FOpenPictureDialog        := TOpenPictureDialog.Create(nil);
  Self.FOpenPictureDialog.Filter := 'jpg|*.jpg|png|*.png|bmp|*.bmp';
  Self.FView.ShowModal;
end;

destructor TImagensController.Destroy;
begin
  FreeAndNil(Self.FFactory);
  FreeAndNil(Self.FImagensModel);
  FreeAndNil(Self.FOpenPictureDialog);
  FreeAndNil(Self.FImagensDao);
  FreeAndNil(Self.FView);
  inherited;
end;

procedure TImagensController.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  Begin
    Key:= #0;
    Self.FView.Perform(Wm_NextDlgCtl,0,0);
  end;
end;

procedure TImagensController.FormShow(Sender: TObject);
var
  LImagens : TObjectList<TImagensModel>;
  aIndex: Integer;
  LStream : TMemoryStream;
  LJpg : TJpegImage;
begin
  LImagens := Self.FImagensDao.GetAll(Self.FCo_Produto);;
  try
    if LImagens.Count > 0 then
    begin
      Self.FView.FDMImagens.Open;
      Self.FView.FDMImagens.EmptyDataSet;
      for aIndex := 0 to Pred(LImagens.Count) do
      begin
        LStream := TMemoryStream.Create;
        LJpg    := TJpegImage.Create;
        try
          Self.FView.FDMImagens.Insert;
          Self.FView.FDMImagensCO_IMAGEM.AsInteger    := LImagens[aIndex].co_imagem;
          Self.FView.FDMImagensCO_PRODUTO.AsInteger   := LImagens[aIndex].co_produto;

          LJpg.LoadFromStream(LImagens[aIndex].im_produto);
          LJpg.SaveToStream(LStream);

          LStream.Position := 0;
          Self.FView.FDMImagensIM_PRODUTO.LoadFromStream(LStream);
          Self.FView.FDMImagensTX_EXTENSAO.AsString   := LImagens[aIndex].tx_extensao;
          Self.FView.FDMImagensDT_CADASTRO.AsDateTime := LImagens[aIndex].dt_cadastro;
          Self.FView.FDMImagens.Post;
        finally
          FreeAndNil(LJpg);
          FreeAndNil(LStream);
        end;
      end;
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    end else begin
      Self.FStatus := TStatus.dsInactive;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FView.FDMImagens.Close;
    end;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);

  finally
    for aIndex := 0 to Pred(LImagens.Count) do
    begin //Removendo os objeto TStrem da lista, senão ocorre vazamento de memória
      var LStreamD : TStream;
      LStreamD := LImagens[aIndex].im_produto;
      FreeAndNil(LStreamD);
    end;
    FreeAndNil(LImagens);
  end;
end;

end.
