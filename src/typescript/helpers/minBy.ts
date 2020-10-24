/**
 * find the smallest element in an array based on a certain criteria
 *
 * @param isSmaller The compare function
 * @param arr The array to search trough
 * @param def Fallback in case of empty arrays
 */
export const minBy = <T>(
    isSmaller: (a: T, b: T) => boolean,
    arr: T[]
): T | null => arr.reduce((acc, curr) => acc === null ? curr : isSmaller(curr, acc) ? curr : acc, null as T | null);