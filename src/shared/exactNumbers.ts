// Catala decimals and money are exact. Nothing here goes through a binary
// float, which holds most decimals only approximately.

// A decimal as the test-case parser spells it: in decimal notation ('0.1',
// '-2.5', '13.' while typing) or, without a finite decimal expansion, as a
// fraction ('1/3').
export const RAT_PATTERN = /^-?\d+(?:\.\d*|\/0*[1-9]\d*)?$/;

export function isValidRat(value: string): boolean {
  return RAT_PATTERN.test(value);
}

// The exact value of a decimal spelling, as numerator and positive
// denominator.
function ratOf(value: string): [bigint, bigint] | undefined {
  if (!isValidRat(value)) return undefined;
  const negative = value.startsWith('-');
  const body = negative ? value.slice(1) : value;
  const slash = body.indexOf('/');
  let num: bigint;
  let den: bigint;
  if (slash >= 0) {
    num = BigInt(body.slice(0, slash));
    den = BigInt(body.slice(slash + 1));
  } else {
    const [units, fraction = ''] = body.split('.');
    num = BigInt(units + fraction);
    den = BigInt(10) ** BigInt(fraction.length);
  }
  return [negative ? -num : num, den];
}

// Whether two decimal spellings denote the same value ('13.' and '13.0',
// '0.5' and '1/2').
export function sameRat(a: string, b: string): boolean {
  const x = ratOf(a);
  const y = ratOf(b);
  return x !== undefined && y !== undefined && x[0] * y[1] === y[0] * x[1];
}

// An amount in cents as units with two digits of cents: '-0.05',
// '123456789012.34'.
export function formatCents(cents: number): string {
  const abs = BigInt(Math.abs(cents));
  const units = abs / BigInt(100);
  const rest = (abs % BigInt(100)).toString().padStart(2, '0');
  return `${cents < 0 ? '-' : ''}${units}.${rest}`;
}

// The cents of an amount written as units with at most two digits of cents
// ('-12.3' is -1230).
export function parseCents(text: string): number {
  const negative = text.startsWith('-');
  const [units, fraction = ''] = (negative ? text.slice(1) : text).split('.');
  const cents = BigInt(units) * BigInt(100) + BigInt(fraction.padEnd(2, '0'));
  return Number(negative ? -cents : cents);
}
