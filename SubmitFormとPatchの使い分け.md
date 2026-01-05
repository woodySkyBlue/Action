# 🎯 **SubmitForm と Patch の使い分け**

**SubmitForm と Patch の使い分け**は非常に重要なテーマです。
ここでは、**本質的な違い → 判断基準 → 実務パターン → 具体例** を説明します。

| 項目 | **SubmitForm** | **Patch** |
|------|----------------|-----------|
| 保存対象 | **フォームの入力値** | **任意の値（フォーム不要）** |
| バリデーション | **自動** | **手動で実装** |
| 添付ファイル | **自動対応** | **自前で構築が必要** |
| 保存後のレコード取得 | **Form.LastSubmit** | **Patch の戻り値** |
| 複雑なロジック | 不向き | **向いている** |
| UI とデータの同期 | **自動** | 手動 |
| 適した用途 | **標準的な CRUD** | **高度な更新・複数テーブル更新** |

---

# 🧩 **1. SubmitForm を使うべき場面**

あなたの開発スタイルに最も合うのは、次のような「フォーム中心の UI」の場合です。

## ✔ フォームで入力 → そのまま保存したい  
```
SubmitForm(Form1)
```

## ✔ バリデーションを自動化したい  
- 必須項目  
- データ型  
- SharePoint / Dataverse の列ルール  
→ すべて自動でチェックされる

## ✔ 添付ファイルを扱う  
フォームなら自動で保存されるため、Patch より圧倒的に安全。

## ✔ 保存後に LastSubmit を使いたい  
```
Form1.LastSubmit.ID
```

## ✔ 保存後の画面遷移や通知を OnSuccess で制御したい  
```
Form1.OnSuccess:
    Navigate(DetailScreen, { rec: Form1.LastSubmit })
```

---

# 🧩 **2. Patch を使うべき場面**

あなたのように「ロジックの自由度」を求める場面では Patch が最適です。

## ✔ 保存前に値を加工したい  
```
Patch(
    DataSource,
    Defaults(DataSource),
    {
        Title: Upper(txtTitle.Text),
        Amount: Value(txtAmount.Text) * 1.1
    }
)
```

## ✔ 複数テーブルを同時に更新したい  
```
Set(
    varParent,
    Patch(ParentTable, Defaults(ParentTable), {...})
);

Patch(ChildTable, Defaults(ChildTable), { ParentID: varParent.ID })
```

## ✔ フォームを使わない UI（独自レイアウト）  
ギャラリーや独自の入力コントロールで構築する場合。

## ✔ 条件分岐が複雑  
```
If(
    chkA.Value,
    Patch(...),
    Patch(...)
)
```

## ✔ 部分更新したい（差分だけ更新）  
```
Patch(DataSource, ThisItem, { Status: "Done" })
```

---

# 🧭 **3. 明浩さん向け：実務での判断基準（最も重要）**

## 🔹 **フォームがある → SubmitForm**  
- 標準 CRUD  
- 添付ファイル  
- バリデーション  
- LastSubmit を使う  
- 保守性を重視

## 🔹 **フォームがない / 複雑な処理 → Patch**  
- 複数テーブル  
- 条件分岐  
- 計算・加工  
- 部分更新  
- カスタム UI

---

# 🧪 **4. 実務パターン（完全版）**

## ✔ パターン1：標準的な保存（SubmitForm）
```
SubmitForm(Form1)
```

Form1.OnSuccess:
```
Notify("保存しました", NotificationType.Success);
Navigate(DetailScreen, { rec: Form1.LastSubmit })
```

---

## ✔ パターン2：保存前に値を加工（Patch）
```
Set(
    varRec,
    Patch(
        Orders,
        Defaults(Orders),
        {
            Title: Upper(txtTitle.Text),
            Total: Value(txtPrice.Text) * Value(txtQty.Text)
        }
    )
);
Navigate(DetailScreen, { rec: varRec })
```

---

## ✔ パターン3：フォームで保存 → 関連テーブルに Patch
Form1.OnSuccess:
```
Patch(
    OrderDetails,
    Defaults(OrderDetails),
    {
        OrderID: Form1.LastSubmit.ID,
        Comment: txtComment.Text
    }
)
```

---

## ✔ パターン4：部分更新（Patch）
```
Patch(Orders, ThisItem, { Status: "Completed" })
```

---

# 🧨 **5. SubmitForm と Patch の混在は注意**

- SubmitForm の直後に Patch を使う場合は **OnSuccess** に書く  
- 保存前に Patch を使うと **LastSubmit が更新されない**

---

# 🎁 **6. 明浩さん向けの最終まとめ**

### ✔ SubmitForm  
- **フォーム中心の UI**  
- **安全・自動・保守性が高い**  
- **LastSubmit が使える**  
- **添付ファイルに強い**

### ✔ Patch  
- **自由度が高い**  
- **複雑なロジックに強い**  
- **フォーム不要**  
- **部分更新・複数テーブル更新に最適**

---

必要であれば、  
- あなたのアプリの保存ロジックを診断して最適化案を作る  
- SubmitForm / Patch のフローチャート  
- 実際のコードをレビューして改善案を提示  

こういったこともできますよ。
