unit Controller.Pedido;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  System.Generics.Collections, Framework.Factory, Model.Pedido, View.Pedido,
  FireDAC.Comp.Client, FireDAC.Stan.Param, Dao.Conexao,
  Vcl.Dialogs, System.Variants, Data.DB, Controller.PedidoItens, Dao.Pedido;

type
  TPedidoController = class
  private
    FView : TFrmPedido;
    FFactory : TFactory;
    FStatus : TStatus;
    FPedidoModel : TPedidoModel;
    FPedidoDao   : TPedidoDao;
    FCampoPesquisa : String;
    FNu_Pedido : Integer;
    FPedido_ItensController : TPedidoItensController;
    procedure btnFecharClick(Sender : TObject);
    procedure btnNovoClick(Sender : TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnGravarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnApagarClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure gridBaseDblClick(Sender: TObject);
    procedure FDMPedidoAfterScroll(DataSet: TDataSet);
    procedure btnProdutosClick(Sender: TObject);
    procedure rdgPesquisaClick(Sender: TObject);
    procedure comboPesqStatusChange(Sender: TObject);
    procedure edtNumeroChange(Sender: TObject);
    procedure FDQClientesPesqAfterScroll(DataSet: TDataSet);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TPedidoController }

procedure TPedidoController.btnApagarClick(Sender: TObject);
begin
  if Application.MessageBox('Confirma a exclusão deste Pedido?', 'Atenção', MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = IDYES then
  begin
    Self.FStatus := TStatus.dsDelete;
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    if Self.FPedidoDao.Delete(Self.FView.FDMPedidoNU_PEDIDO.AsInteger) then
    begin
      Self.FFactory.ClearControls(Self.FView);
      Self.FormShow(Sender);
    end
    else
      Self.FormShow(Sender);
  end;
end;

procedure TPedidoController.btnCancelarClick(Sender: TObject);
begin
  if Self.FStatus = TStatus.dsEdit then
  begin
    Self.FFactory.ClearControls(Self.FView);
    Self.FView.edtCodigo.Text        := FormatFloat('000000', Self.FView.FDMPedidoNU_PEDIDO.AsInteger);
    Self.FView.comboCliente.KeyValue := Self.FView.FDMPedidoCO_EMPRESA.AsInteger;
    Self.FView.edtValorPedido.Text   := FormatFloat('#,##0.00', Self.FView.FDMPedidoVL_PEDIDO.AsCurrency);

    if Self.FView.FDMPedidoIN_SITUACAO.AsString = 'G' then //Gerado
      Self.FView.comboStatus.ItemIndex := 1
    else if Self.FView.FDMPedidoIN_SITUACAO.AsString = 'L' then //Liberado
      Self.FView.comboStatus.ItemIndex := 1
    else
      Self.FView.comboStatus.ItemIndex := 0; //Fechado

    Self.FStatus := TStatus.dsBrowse;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  end else if Self.FStatus = TStatus.dsInsert then
  begin
    Self.FFactory.ClearControls(Self.FView);
    Self.FStatus := TStatus.dsBrowse;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  end;
end;

procedure TPedidoController.btnEditarClick(Sender: TObject);
begin
  Self.FStatus := TStatus.dsEdit;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
  Self.FView.edtCodigo.Text        := FormatFloat('000000', Self.FView.FDMPedidoNU_PEDIDO.AsInteger);
  Self.FView.comboCliente.KeyValue := Self.FView.FDMPedidoCO_EMPRESA.AsInteger;
  Self.FView.edtValorPedido.Text   := FormatFloat('#,##0.00', Self.FView.FDMPedidoVL_PEDIDO.AsCurrency);

  if Self.FView.FDMPedidoIN_SITUACAO.AsString = 'G' then
    Self.FView.comboStatus.ItemIndex := 1
  else if Self.FView.FDMPedidoIN_SITUACAO.AsString = 'L' then
    Self.FView.comboStatus.ItemIndex := 2
  else
    Self.FView.comboStatus.ItemIndex := 0;
end;

procedure TPedidoController.btnFecharClick(Sender: TObject);
begin
  Self.FView.Close;
end;

procedure TPedidoController.btnGravarClick(Sender: TObject);
var
  aListError : TStringList;
begin
  aListError := TStringList.Create;
  aListError.Add('Lista de erro(s):');
  aListError.Add('');
  try
    if String(Self.FView.comboCliente.Text).IsEmpty then
      aListError.Add('Cliente');
    if String(Self.FView.comboStatus.Text).IsEmpty then
      aListError.Add('Status');

    if aListError.Count > 2 then
    begin
      aListError.Add('');
      aListError.Add('O(s) campo(s) listado(s) acima são de preenchimento obrigatório.');

      Application.MessageBox(PChar(aListError.Text), 'Atenção', MB_OK+MB_ICONERROR);
      Self.FView.comboCliente.SetFocus;
    end else
    begin
      if Self.FStatus = TStatus.dsEdit then
      begin
        Self.FPedidoModel.nu_pedido := Self.FView.FDMPedidoNU_PEDIDO.AsInteger;
        Self.FPedidoModel.dt_alteracao := Self.FFactory.GetCurrentTimeStamp(DMConexao.Conexao);
      end
      else
        Self.FPedidoModel.nu_pedido  := StrToInt(Self.FView.edtCodigo.Text);

      Self.FPedidoModel.co_empresa   := Self.FView.comboCliente.KeyValue;
      Self.FPedidoModel.in_situacao  := Copy(Self.FView.comboStatus.Text, 1, 1);
      Self.FPedidoModel.vl_pedido    := 0.00;

      if Self.FStatus = TStatus.dsInsert then
      begin

        Self.FPedidoDao.Insert(Self.FPedidoModel);

        if not Self.FView.FDMPedido.Active then
          Self.FView.FDMPedido.Open;

        Self.FView.FDMPedido.Insert;
        Self.FView.FDMPedidoNU_PEDIDO.AsInteger    := FPedidoModel.nu_pedido;
        Self.FView.FDMPedidoCO_EMPRESA.AsInteger   := FPedidoModel.co_empresa;
        Self.FView.FDMPedidoIN_SITUACAO.AsString   := FPedidoModel.in_situacao;
        Self.FView.FDMPedidoVL_PEDIDO.AsCurrency   := 0.00;
        Self.FView.FDMPedidoDT_CADASTRO.AsDateTime := Self.FPedidoDao.GetDateTime;
        Self.FView.FDMPedido.Post;

      end else if Self.FStatus = TStatus.dsEdit then
        Self.FPedidoDao.Update(Self.FPedidoModel);

      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    end;
  finally
    FreeAndNil(aListError);
  end;
end;

procedure TPedidoController.btnNovoClick(Sender: TObject);
begin
  Self.FFactory.ClearControls(Self.FView);
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
  Self.FView.comboCliente.SetFocus;
  Self.FView.edtCodigo.Text        := FormatFloat('000000', Self.FPedidoDao.GetNewID('INC_NU_PEDIDO'));
  Self.FView.comboStatus.ItemIndex := 1;
  Self.FView.edtValorPedido.Text   := '0,00';
  Self.FStatus := TStatus.dsInsert;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
end;

procedure TPedidoController.btnProdutosClick(Sender: TObject);
begin
  if Self.FView.FDMPedido.RecordCount = 0 then
    Exit;

  if String(Self.FView.comboCliente.Text).IsEmpty then
  begin
    Application.MessageBox('Selecione o Pedido ao qual deseja incluir Produtos!', 'Atenção', MB_OK+MB_ICONWARNING);
    Abort;
  end;

  try
    FNu_Pedido := Self.FView.FDMPedidoNU_PEDIDO.AsInteger;
    Self.FPedido_ItensController := TPedidoItensController.Create(FNu_Pedido);
    Self.FormShow(Sender);
  finally
    FreeAndNil(Self.FPedido_ItensController);
  end;
end;

procedure TPedidoController.comboPesqStatusChange(Sender: TObject);
begin
  Self.FView.FDMPedido.Filtered := False;
  Self.FView.FDMPedido.DisableControls;
  Self.FView.FDMPedido.AfterScroll := nil;
  Self.FView.FDMPedido.Filter   := Self.FCampoPesquisa + ' = ' +
                                   QuotedStr(Copy(Self.FView.comboPesqStatus.Text, 1, 1));
  Self.FView.FDMPedido.Filtered := True;;
  Self.FView.FDMPedido.AfterScroll := Self.FDMPedidoAfterScroll;
  Self.FView.FDMPedido.EnableControls;
end;

constructor TPedidoController.Create;
begin
  Self.FFactory                          := TFactory.Create;
  Self.FPedidoModel                      := TPedidoModel.Create;
  Self.FPedidoDao                        := TPedidoDao.Create;
  Self.FView                             := TFrmPedido.Create(nil);
  Self.FView.KeyPreview                  := True;
  Self.FView.btnFechar.OnClick           := Self.btnFecharClick;
  Self.FView.btnNovo.OnClick             := Self.btnNovoClick;
  Self.FView.btnEditar.OnClick           := Self.btnEditarClick;
  Self.FView.btnGravar.OnClick           := Self.btnGravarClick;
  Self.FView.OnShow                      := Self.FormShow;
  Self.FView.OnKeyPress                  := Self.FormKeyPress;
  Self.FView.btnCancelar.OnClick         := Self.btnCancelarClick;
  Self.FView.btnApagar.OnClick           := Self.btnApagarClick;
  Self.FView.gridBase.OnDblClick         := Self.gridBaseDblClick;
  Self.FView.FDMPedido.AfterScroll       := Self.FDMPedidoAfterScroll;
  Self.FView.btnProdutos.OnClick         := Self.btnProdutosClick;
  Self.FView.rdgPesquisa.OnClick         := Self.rdgPesquisaClick;
  Self.FView.comboPesqStatus.OnChange    := Self.comboPesqStatusChange;
  Self.FView.edtNumero.OnChange          := Self.edtNumeroChange;
  Self.FView.FDQClientesPesq.AfterScroll := Self.FDQClientesPesqAfterScroll;
  Self.FView.btnNovo.ShowHint            := True;
  Self.FView.btnNovo.Hint                := 'Adicionar um novo registro';
  Self.FView.btnEditar.ShowHint          := True;
  Self.FView.btnEditar.Hint              := 'Editar o registro corrente';
  Self.FView.btnGravar.ShowHint          := True;
  Self.FView.btnGravar.Hint              := 'Gravar o registro corrente';
  Self.FView.btnCancelar.ShowHint        := True;
  Self.FView.btnCancelar.Hint            := 'Cnacelar a operação corrente';
  Self.FView.btnApagar.ShowHint          := True;
  Self.FView.btnApagar.Hint              := 'Apagar o registro corrente';
  Self.FView.btnFechar.ShowHint          := True;
  Self.FView.btnFechar.Hint              := 'Fechar';
  Self.FStatus                           := TStatus.dsInactive;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FView.ShowModal;
end;

destructor TPedidoController.Destroy;
begin
  FreeAndNil(Self.FFactory);
  FreeAndNil(Self.FPedidoModel);
  FreeAndNil(Self.FPedidoDao);
  FreeAndNil(Self.FView);
  inherited;
end;

procedure TPedidoController.edtNumeroChange(Sender: TObject);
begin
  if (Self.FStatus = TStatus.dsBrowse) or (String(Self.FView.edtNumero.Text).IsEmpty) then
    Exit;
  Self.FView.FDMPedido.Filtered := False;
  Self.FView.FDMPedido.DisableControls;
  Self.FView.FDMPedido.AfterScroll := nil;
  Self.FView.FDMPedido.Filter   := Self.FCampoPesquisa + ' = ' +
                                   QuotedStr(Self.FView.edtNumero.Text);
  Self.FView.FDMPedido.Filtered := True;;
  Self.FView.FDMPedido.AfterScroll := Self.FDMPedidoAfterScroll;
  Self.FView.FDMPedido.EnableControls;
end;

procedure TPedidoController.FDMPedidoAfterScroll(DataSet: TDataSet);
begin
  if Self.FStatus <> TStatus.dsBrowse then
    Exit;
  Self.FView.edtCodigo.Text        := FormatFloat('000000', DataSet.FieldByName('NU_PEDIDO').AsInteger);
  Self.FView.comboCliente.KeyValue := DataSet.FieldByName('CO_EMPRESA').AsInteger;
  Self.FView.edtValorPedido.Text   := FormatFloat('#,##0.00', DataSet.FieldByName('VL_PEDIDO').AsCurrency);

  if DataSet.FieldByName('IN_SITUACAO').AsString = 'G' then
    Self.FView.comboStatus.ItemIndex := 1
  else if DataSet.FieldByName('IN_SITUACAO').AsString = 'L' then
    Self.FView.comboStatus.ItemIndex := 2
  else
    Self.FView.comboStatus.ItemIndex := 0;
end;

procedure TPedidoController.FDQClientesPesqAfterScroll(DataSet: TDataSet);
begin
  if String(Self.FView.comboPesqCliente.Text).IsEmpty then
    Exit;
  Self.FView.FDMPedido.Filtered := False;
  Self.FView.FDMPedido.DisableControls;
  Self.FView.FDMPedido.AfterScroll := nil;
  Self.FView.FDMPedido.Filter   := Self.FCampoPesquisa + ' = ' +
                                   QuotedStr(Self.FView.comboPesqCliente.KeyValue);
  Self.FView.FDMPedido.Filtered := True;;
  Self.FView.FDMPedido.AfterScroll := Self.FDMPedidoAfterScroll;
  Self.FView.FDMPedido.EnableControls;
end;

procedure TPedidoController.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  Begin
    Key:= #0;
    Self.FView.Perform(Wm_NextDlgCtl,0,0);
  end;
end;

procedure TPedidoController.FormShow(Sender: TObject);
var
  LPedidos : TObjectList<TPedidoModel>;
  aIndex: Integer;
begin
  LPedidos := Self.FPedidoDao.GetAll;
  try
    if LPedidos.Count > 0 then
    begin
      Self.FView.FDMPedido.Open;
      Self.FView.FDMPedido.EmptyDataSet;
      for aIndex := 0 to Pred(LPedidos.Count) do
      begin
        Self.FView.FDMPedido.Insert;
        Self.FView.FDMPedidoNU_PEDIDO.AsInteger    := LPedidos[aIndex].nu_pedido;
        Self.FView.FDMPedidoCO_EMPRESA.AsInteger   := LPedidos[aIndex].co_empresa;
        Self.FView.FDMPedidoIN_SITUACAO.AsString   := LPedidos[aIndex].in_situacao;
        Self.FView.FDMPedidoVL_PEDIDO.AsCurrency   := LPedidos[aIndex].vl_pedido;
        Self.FView.FDMPedidoDT_CADASTRO.AsDateTime := LPedidos[aIndex].dt_cadastro;

        if not Self.FFactory.DateTimeIsNull(LPedidos[aIndex].dt_alteracao) then
          Self.FView.FDMPedidoDT_ALTERACAO.AsDateTime  := LPedidos[aIndex].dt_alteracao;
        Self.FView.FDMPedido.Post;
      end;
      if Self.FNu_Pedido > 0 then
      begin
        if Self.FView.FDMPedido.Locate('NU_PEDIDO', Self.FNu_Pedido, []) then
          Self.FNu_Pedido := 0;
      end
      else
        Self.FView.FDMPedido.First;
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    end else begin
      Self.FStatus := TStatus.dsInactive;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FView.FDMPedido.Close;
    end;
    Self.FPedidoDao.ToLoadComboCliente(Self.FView.FDQClientes);
    Self.FPedidoDao.ToLoadComboCliente(Self.FView.FDQClientesPesq);
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    if LPedidos.Count > 0 then
      Self.FView.rdgPesquisa.Enabled := True;
  finally
    FreeAndNil(LPedidos);
  end;
end;

procedure TPedidoController.gridBaseDblClick(Sender: TObject);
begin
  Self.btnEditarClick(Sender);
end;

procedure TPedidoController.rdgPesquisaClick(Sender: TObject);
begin
  case Self.FView.rdgPesquisa.ItemIndex of
    0 : begin //Status do Pedido
          Self.FView.comboPesqStatus.Visible  := True;
          Self.FView.comboPesqStatus.Enabled  := True;
          Self.FView.comboPesqCliente.Visible := False;
          Self.FView.edtNumero.Visible        := False;
          Self.FCampoPesquisa                 := 'IN_SITUACAO';
        end;
    1 : begin //Número do Pedido
          Self.FView.comboPesqStatus.Visible  := False;
          Self.FView.comboPesqCliente.Visible := False;
          Self.FView.edtNumero.Visible        := True;
          Self.FView.edtNumero.Enabled         := True;
          Self.FCampoPesquisa                 := 'NU_PEDIDO';
        end;
    2 : begin //Cliente
          Self.FView.comboPesqStatus.Visible  := False;
          Self.FView.comboPesqCliente.Visible := True;
          Self.FView.comboPesqCliente.Enabled := True;
          Self.FView.edtNumero.Visible        := False;
          Self.FCampoPesquisa                 := 'CO_EMPRESA';
        end;
  end;
end;

end.
