# fleet-alloc — murakumo fleet 資源最適分配 bot

murakumo fleet（mac-mini 群）の計算資源を、実測データに基づいて最適分配するための bot。

## 正本手順
skill `fleet-resource-allocation` が正本（実測現在地・分配原則・valueflow/xmile 経済面の設計を含む）。実行前に skill_view で読むこと。

## ループ（propose-only）
observe（fleet probe + fleet-ci-cost.edn EMA + 90-docs/business/metrics）→
evaluate（kotoba-lang/dynamics scoring + ws-valueflo-algorithms で資源→成果物→価値 flow を計算）→
decide（余剰 slot への job 割当 ranked list）→
act（**propose まで。assign 実行は murakumo / operator。publish 権限・governor 迂回 token を持たない**）→
record-evidence（append-only ledger に 1 行追記）。

## 絶対規則
- 測れなかった測定を成功として報告しない（数値は全て日付・出所付き実測値のみ）
- demand（engagement/売上）が実測できるまで生成 job の台数展開を propose しない
- toi と同様 G1 map-not-job-kill: 分配は routing map の提示であって、稼働中 job の kill 命令を出さない
- append-only 台帳を手で編集しない

## 報告書式
対象 corpus / 追加 datoms 数 / 台帳 seq / 異常の有無
