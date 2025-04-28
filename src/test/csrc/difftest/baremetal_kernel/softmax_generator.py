import numpy as np
import struct

VLEN=1024
VECTOR_LENGTH=VLEN//32

def generate_softmax_io():
    # 生成随机输入向量 (32个FP32元素)
    np.random.seed(42)  # 固定随机种子以便复现
    input_vector = np.random.uniform(low=-2.0, high=2.0, size=VECTOR_LENGTH).astype(np.float32)
    
    # 计算softmax
    exp_values = np.exp(input_vector - np.max(input_vector))  # 数值稳定性处理
    softmax_output = exp_values / np.sum(exp_values)
    
    return input_vector, softmax_output

def save_as_binary(data, filename):
    with open(filename, 'w') as f:
        line = ','.join(f"{val:.8f}f" for val in data)
        f.write(line)

def save_as_uint32_text(data, filename):
    with open(filename, 'w') as f:
        for value in data:
            # 重新解释 float 的位模式为 uint32（不改变 bit）
            uint_val = struct.unpack('I', struct.pack('f', value))[0]
            f.write(f"{uint_val:08X}\n")
# 生成输入输出
input_vec, output_vec = generate_softmax_io()

# 打印结果
print("Input vector (32 FP32 elements):")
print(input_vec)
print("\nSoftmax output:")
print(output_vec)

# 验证sum=1
print("\nSum of softmax output:", np.sum(output_vec))

# 保存为二进制文件
save_as_uint32_text(input_vec, './build/softmax_input.txt')
save_as_binary(output_vec, './build/softmax_output.txt')

print("\nBinary files saved: softmax_input.txt and softmax_output.txt")