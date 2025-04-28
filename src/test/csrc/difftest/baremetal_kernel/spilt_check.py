import struct

# 原始16进制字符串（去掉0x前缀）
hex_str = "3bc23a383d0b71f23b6c7c003d03418b3cc003fd3bda3f483d8de3b33c98286d3c548fdb3c1df6af3bab8e4a3d0dde6e3c1d62dc3c8a29313cc845063c25cba53bcc7f203bcb35683be596b83dab6b723e1489923b553d743d50779d3d07e7873dc42ee83b77bfa83bb742d03bb747593d0693563d65666a3e098f293c5ba168"
# 分解成每8个字符一段
hex_list = [hex_str[i:i+8] for i in range(0, len(hex_str), 8)]

# 将每段16进制转换为FP32
fp32_list = []
for h in hex_list:
    # 将16进制字符串转换为字节（小端序）
    bytes_data = bytes.fromhex(h)
    # 使用struct.unpack转换为浮点数
    fp32 = struct.unpack('>f', bytes_data)[0]  # 大端序
    fp32_list.append(fp32)

# 打印结果
for i, (h, fp) in enumerate(zip(hex_list, fp32_list)):
    print(f"{i+1}: 0x{h} -> {fp}")