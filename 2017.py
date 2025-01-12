def to_binary(value, bits):
    binary_str = ""
    for _ in range(bits):
        binary_str = (str(value % 2)
                      + binary_str)
        value //= 2
    return binary_str.zfill(bits)

day, month, year = 2, 7, 2017
binary_date = (
    to_binary(day, 5) +
    to_binary(month, 4) +
    to_binary(year, 12))

print(binary_date)