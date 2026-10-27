# RV32IM Tomasulo CPU

这是一个纯 Chisel 实现、从零搭建的 RV32IM 乱序处理器项目。项目不复制助教提供的
Chisel 模板，只遵循课程要求的顶层接口、RV32IM 指令语义和测试流程。

## 目标

- 指令集：RV32I 用户态指令和完整的 M 扩展。
- 微架构：Tomasulo 乱序发射、乱序执行、按序提交。
- 流水级：取指、译码/分派、发射、执行/访存、提交五个逻辑流水级。乘除法、
  Cache 和总线访问可以占用多个周期；五级描述的是流水组织，不限制执行单元延迟。
- 发射宽度：1、2、4 可配置。
- 主要参数：ROB 深度、物理寄存器数量、保留站深度、Cache 容量和相联度。
- 外部接口：`student_top`，AXI4-Lite 主端，4 字节对齐访存。

架构说明和里程碑见 [docs/architecture.md](docs/architecture.md)。

## 仓库分工

- `vibe`：唯一可编辑的 Chisel 源码仓库。
- `CPU-submit`：由 Chisel 生成的 Verilog/SystemVerilog，供 OJ 提交。
- `RISC-V-CPU-2026`：课程框架和官方测试点，只拉取，不 push。

Chisel 是硬件设计的唯一事实来源。不要在 `CPU-submit` 中手工修改生成的 RTL。

## 开发环境

项目在 WSL Ubuntu 22.04 下开发、生成、仿真和综合。课程工具包位于项目总目录：

```text
/mnt/d/CPU-vibe/cpu2026-tools-x86_64.AppImage
```

课程 AppImage 已包含 Verilator、Yosys、ABC 和 OpenSTA；Chisel 生成流程需要独立安装
Chisel 构建工具。所有 Git 操作使用 WSL 中的 `git`。

## 验证约定

1. 每个功能提交先在 `vibe` 中通过 Chisel 编译和单元测试。
2. 从同一提交生成 RTL，放入 `CPU-submit`，再使用课程框架做官方测试。
3. 正确性测试默认周期上限不足，完整回归使用至少
   `MAX_CYCLES=5000000`。
4. 官方测试没有覆盖完整 RV32IM，因此项目同时维护定向 ISA 测试。
5. 性能结果同时记录 IPC、动态周期数、面积、关键路径和目标频率配置。
