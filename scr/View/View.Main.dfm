object FrmMain: TFrmMain
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Controle de Pedidos'
  ClientHeight = 583
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 583
    Align = alLeft
    BevelInner = bvLowered
    Color = clMedGray
    Padding.Left = 1
    Padding.Top = 1
    Padding.Right = 1
    Padding.Bottom = 1
    ParentBackground = False
    TabOrder = 0
    ExplicitHeight = 571
    object btnSair: TSpeedButton
      Left = 3
      Top = 544
      Width = 179
      Height = 36
      Align = alBottom
      Caption = 'Sair'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnSairClick
      ExplicitLeft = 5
      ExplicitTop = 5
      ExplicitWidth = 175
    end
    object btnEmpresa: TSpeedButton
      Left = 3
      Top = 3
      Width = 179
      Height = 36
      Align = alTop
      Caption = 'Empresa'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnEmpresaClick
      ExplicitLeft = 5
      ExplicitTop = 5
      ExplicitWidth = 175
    end
    object btnUsuario: TSpeedButton
      Left = 3
      Top = 111
      Width = 179
      Height = 36
      Align = alTop
      Caption = 'Usu'#225'rio do Sistema'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnUsuarioClick
      ExplicitLeft = 5
      ExplicitTop = 5
      ExplicitWidth = 175
    end
    object btnPedido: TSpeedButton
      Left = 3
      Top = 75
      Width = 179
      Height = 36
      Align = alTop
      Caption = 'Pedido'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnPedidoClick
      ExplicitLeft = 5
      ExplicitTop = 5
      ExplicitWidth = 175
    end
    object btnProduto: TSpeedButton
      Left = 3
      Top = 39
      Width = 179
      Height = 36
      Align = alTop
      Caption = 'Produto'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnProdutoClick
      ExplicitLeft = 5
      ExplicitTop = 5
      ExplicitWidth = 175
    end
  end
  object MainMenu1: TMainMenu
    Left = 288
    Top = 128
    object Cadastro1: TMenuItem
      Caption = 'Cadastro'
      object Empresa1: TMenuItem
        Caption = 'Empresa'
        OnClick = Empresa1Click
      end
      object Produto1: TMenuItem
        Caption = 'Produto'
        OnClick = Produto1Click
      end
      object Pedido1: TMenuItem
        Caption = 'Pedido'
        OnClick = Pedido1Click
      end
      object Usurio1: TMenuItem
        Caption = 'Usu'#225'rio'
        OnClick = Usurio1Click
      end
    end
  end
end
