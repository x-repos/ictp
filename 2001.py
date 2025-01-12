def to_binary(value, bits):
    binary_str = ""
    for _ in range(bits):
        binary_str = (str(value % 2)
                      + binary_str)
        value //= 2
    return binary_str.zfill(bits)

day, month, year = 13, 5, 2001
binary_date = (
    to_binary(day, 5) +
    to_binary(month, 4) +
    to_binary(year, 12))

print(binary_date)