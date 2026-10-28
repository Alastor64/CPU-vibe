# 验证记录

## 官方正确性

使用 4 发射默认配置和 `MAX_CYCLES=30000000` 完成全部回归：

```text
Results: 19 passed, 0 failed
```

覆盖递归、函数调用、数组、多维数组、整数乘除、循环、分支和退出 MMIO。

另外使用 RISC-V GCC 编译了 `testcases/directed_rv32im`，覆盖官方程序未直接使用的
`slt/sltu/slti/sltiu/or/ori/and/srai`、有符号/无符号半字与字节访存，以及
`mulh/mulhsu/mulhu/divu/remu`。该定向下程序返回 0。

## 参数化检查

- `ISSUE_WIDTH=1`：功能基线通过。
- `ISSUE_WIDTH=2`：表达式、GCD、多维数组等测试通过。
- `ISSUE_WIDTH=4`：官方正确性全集通过。
- 4 KiB、16 KiB 和 32 KiB D-Cache 均完成仿真；较大的 Cache 能显著改善数组基准，
  但当前寄存器阵列实现不适合综合，因此默认保留 4 KiB。

## 当前性能

4 发射、4 KiB D-Cache、10 周期外部内存延迟：

| benchmark | IPC |
| --- | ---: |
| perf_median | 0.2659 |
| perf_multiply | 1.0836 |
| perf_qsort | 0.1385 |
| perf_rsort | 0.1244 |
| perf_towers | 0.1941 |
| perf_vvadd | 0.1878 |
| GEOMEAN | 0.2378 |

32 KiB D-Cache 将 GEOMEAN 提升到约 0.358，但综合未收敛，暂不作为提交默认值。
后续必须完成 SRAM 化 Cache 和多端口数据访问后再优化 IPC/面积/频率。

## 数据 Cache SRAM 化

`feature/sram-data-cache` 将 D-Cache 行数据迁移到课程 `sram_fakeram`，保留标签和有效位
寄存器。该配置同样通过官方正确性 19/19 和完整 ISA 定向测试。同步 SRAM 增加了一级
读延迟，当前 GEOMEAN 为 0.2279，略低于寄存器阵列版本；下一步需要将 I-Cache 也
SRAM 化，并通过多端口或 banking 恢复访存吞吐。

`feature/sram-instruction-cache` 进一步把 I-Cache 行数据迁移到 `sram_fakeram`，并用
单行缓冲保持多字取指。最终配置再次通过官方正确性 19/19，GEOMEAN 为 0.2274。

`feature/shared-muldiv` 将 4 份组合乘除逻辑压缩为一个共享 M 扩展单元。乘法延迟
2 拍，除法/取余延迟 34 拍，官方正确性 19/19 和完整 ISA 定向测试通过。除法密集型
程序周期数明显增加，后续通过 radix-4/8 迭代或更高基数的共享除法器恢复吞吐。

乘法器随后进一步合并为单一无符号 32×32 阵列，`MULH/MULHSU` 使用符号位驱动的
32 位减法修正高位。ASAP7 快速映射下，修正后的完整乘法器面积约 `941.07 μm²`，
相比之前三套乘法表达式的 `2,268.66 μm²` 减少约 58.5%。

`feature/pipelined-wallace-multiplier` 将组合乘法拆成两级：第一拍生成两个 16 位
limb 的 2×2 部分积并用 Wallace 3:2 压缩树写入 sum/carry，第二拍完成 64 位加法。
乘法从接收到写回仍为 2 拍。乘法器单测覆盖 1049 组边界/随机输入，M 扩展单测覆盖
48 组指令边界；生成的 4 发射 RTL 通过完整 ISA 定向程序和官方正确性 19/19。
