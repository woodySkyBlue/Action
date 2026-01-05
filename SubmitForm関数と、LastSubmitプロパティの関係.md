# 🚀 **SubmitForm関数と、LastSubmitプロパティの関係**

# ✅ **SubmitForm は「関数」**

### ✔ 役割  
フォームに入力された内容を **データソースへ保存する処理を実行する命令**。

### ✔ 書き方  
```
SubmitForm(Form1)
```

### ✔ 特徴  
- 実行すると保存処理が走る  
- 成功すると **OnSuccess** が発火  
- 失敗すると **OnFailure** が発火  
- 保存後に **Form1.LastSubmit** が更新される  

---

# ✅ **LastSubmit は「フォームのプロパティ」**

### ✔ 役割  
**直前に SubmitForm が成功したときに保存されたレコードを保持する値**。

### ✔ 書き方  
```
Form1.LastSubmit
Form1.LastSubmit.ID
```

### ✔ 特徴  
- SubmitForm が成功したときだけ更新される  
- レコード（1行分のデータ）を返す  
- 新規作成でも編集でも使える  
- Patch では更新されない（Patch の戻り値を使う）

---

# 🎯 **関数とプロパティの関係を図で表すと…**

```
SubmitForm(Form1)   ← 実行する命令（関数）
       ↓ 成功
Form1.LastSubmit    ← 保存されたレコード（プロパティ）
```

この関係を理解しておくと、保存後の処理がとても組みやすくなります。

---

# 🧪 **実務での典型的な組み合わせ**

### ■ 保存 → 確認画面へ遷移
Form1.OnSuccess に：

```
Navigate(ConfirmScreen, { rec: Form1.LastSubmit })
```

### ■ 保存した ID を通知
```
Notify($"保存しました（ID: {Form1.LastSubmit.ID}）", NotificationType.Success)
```

---

# 🧭 **明浩さん向けのまとめ**

- **SubmitForm** → 保存処理を「実行する」関数  
- **LastSubmit** → 保存されたレコードを「保持する」プロパティ  

この2つはセットで使うと威力を発揮します。  
あなたのようにロジックの整合性を重視する方には、まさに理想的な組み合わせです。

---

必要であれば、  
- SubmitForm / Patch の使い分け  
- LastSubmit を使った保存後フローのテンプレート  
- フォームの OnSuccess / OnFailure のベストプラクティス  

などもまとめますよ。
