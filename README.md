# 决策理论与方法交互式学习网页

本仓库包含《决策理论与方法（第三版）》（郭文强、孙世勋、郭立夫主编）各章对应的交互式学习网页，使用 R + Shiny 编写。

## 项目结构

| 目录 | 对应章节 | 内容说明 |
|---|---|---|
| `Decision_Overview` | 第一章 决策分析概述 | 打鸡蛋案例互动模拟 |
| `Decision_Certainty` | 第二章 确定型决策分析 | 自制还是外购决策（修复提升） |
| `Decision_Risk` | 第三章 风险型决策分析 | 产品包装改进：先验决策 + 贝叶斯后验决策 |
| `Decision_Uncertainty` | 第四章 不确定型决策分析 | 不确定型与风险型决策准则（修复提升） |
| `Decision_AHP` | 第五章 多目标决策分析 | AHP 层次分析法：利润分配案例 |
| `Q_Learning` | 第六章 序贯决策分析 | Q-learning 房间脱出（修复提升） |
| `Decision_Game` | 第七章 竞争型决策分析——博弈论 | 纳什均衡求解器 |
| `Decision_DSS` | 第八章 决策支持系统 | DSS 架构认知互动测试 |
| `Decision_BigData` | 第九章 大数据分析与决策 | K-means 客户细分演示 |

## 如何运行

本机 R 已安装在 `D:\Program Files\R\R-4.6.1`。我已使用该 R 环境完成以下工作：

- 语法解析测试：全部 9 个 `app.R` 均通过 `parse()`；
- 加载测试：全部 9 个应用均可成功 `source`；
- 启动测试：`Decision_Overview`、`Decision_Certainty`、`Decision_AHP`、`Q_Learning` 均能正常监听端口（无启动错误）。

每个子目录都是一个独立的 Shiny 应用。进入对应目录后，在 R 中运行：

```r
shiny::runApp("app.R")
```

或在命令行中：

```bash
"D:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e "shiny::runApp('app.R', launch.browser=FALSE, port=7456)"
```

所需依赖包（`shiny`、`DT`、`ggplot2`、`tidyr`、`rhandsontable`、`visNetwork`）已安装到 R 用户库。如以后在新环境运行，各 `app.R` 文件顶部仍保留 `install.packages()` 提示。

## 对原有项目的修复与提升

### 1. Decision_Certainty（确定型决策）
- 新增 IRR、静态回收期、动态回收期计算
- 新增临界需求量可视化
- 新增参数敏感性龙卷风图
- 新增现金流累计现值列
- 新增结果下载功能
- 优化 UI 与响应式布局

### 2. Decision_Uncertainty（不确定型决策）
- 新增教材默认收益矩阵
- 新增风险型决策：概率输入、期望收益、方差、标准差
- 新增完全情报价值 EVPI 计算
- 新增各方案收益分布图与准则对比图
- 改进矩阵输入与结果展示

### 3. Q_Learning（序贯决策）
- 新增 epsilon-greedy 探索/利用机制
- 新增学习曲线与到达目标步数曲线
- 新增当前最优策略显示
- 新增动作选择类型（探索/利用）日志
- 保留并逐步改进原有房间脱出案例

## 新增项目说明

- **Decision_Risk**：基于第三章习题 2（产品包装改进），实现先验决策、贝叶斯后验概率、EVPI、EVSI 及敏感性分析。
- **Decision_AHP**：基于第五章习题 2（利润分配方案），实现三层 AHP 判断矩阵输入、权重计算、一致性检验与层次总排序。
- **Decision_Game**：基于第七章习题 4（声明博弈），实现任意 2×2–5×5 双矩阵博弈的纯策略纳什均衡求解，以及 2×2 博弈的混合策略均衡计算。
- **Decision_Overview / Decision_DSS / Decision_BigData**：针对第一、八、九章没有数值型综合性习题的特点，分别设计概念性互动演示。

## 版权声明

各应用程序采用 **CC BY-NC-SA 4.0**（署名—非商业性使用—相同方式共享 4.0 国际）许可协议授权。如发现缺陷或错误，请发送邮件至 gengc25@hotmail.com。
