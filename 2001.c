#include <stdio.h>
#include <string.h>

// Function to convert an integer to binary with a fixed number of bits
void to_binary(int value, int bits, char *binary_str) {
    for (int i = bits - 1; i >= 0; i--) {
        binary_str[i] = (value % 2) + '0'; // Extract the bit and convert to char
        value /= 2;
    }
    binary_str[bits] = '\0'; // Null-terminate the string
}

int main() {
    int day = 13, month = 5, year = 2001;

    // Buffers to store binary strings
    char binary_day[6];  // 5 bits + null terminator
    char binary_month[5]; // 4 bits + null terminator
    char binary_year[13]; // 12 bits + null terminator
    char binary_date[22]; // Total length: 5 + 4 + 12 + null terminator

    // Convert components to binary
    to_binary(day, 5, binary_day);
    to_binary(month, 4, binary_month);
    to_binary(year, 12, binary_year);

    // Concatenate the binary strings
    strcpy(binary_date, binary_day);
    strcat(binary_date, binary_month);
    strcat(binary_date, binary_year);

    // Print the final binary representation
    printf("%s\n", binary_date);

    return 0;
}
