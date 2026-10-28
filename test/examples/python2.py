# file_b.py
def calculate_sum(a, b):
    # 使用更简化的方式计算和
    return a + b


def greet(name):
    print(f"Hello, {name}!")


def multiply(a, b):
    # 新增的乘法函数
    return a * b


if __name__ == "__main__":
    print(calculate_sum(3, 5))
    print(multiply(2, 4))
    greet("Bob")
