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
