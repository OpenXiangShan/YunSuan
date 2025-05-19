import re

def count_instructions(log_file):
    instruction_count = {}

    # 使用正则表达式匹配指令
    #instruction_pattern = re.compile(r'0x[0-9a-f]+\s+\(0x[0-9a-f]+\)\s+([a-zA-Z0-9_]+)')
    instruction_pattern = re.compile(r'^\s*[0-9a-f]+:\s+[0-9a-f]+\s+([a-zA-Z0-9_.]+)')

    with open(log_file, 'r') as file:
        for line in file:
            # 查找匹配的指令
            match = instruction_pattern.search(line)
            if match:
                instruction = match.group(1)
                # 更新指令计数
                if instruction in instruction_count:
                    instruction_count[instruction] += 1
                else:
                    instruction_count[instruction] = 1

    return instruction_count

def main():
    log_file = 'baremetal_kernel/build/softmax.txt'  # 替换为你的日志文件路径
    counts = count_instructions(log_file)

    # 按首字母顺序排序
    sorted_instructions = sorted(counts.items(), key=lambda x: x[0])

    # 打印统计结果
    print("Instruction Execution Counts:")
    for instruction, count in sorted_instructions:
        print(f"{instruction}: {count}")

if __name__ == "__main__":
    main()