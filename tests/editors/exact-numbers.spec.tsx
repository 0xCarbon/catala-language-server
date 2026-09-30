import { describe, it, expect, vi, beforeEach } from 'vitest';
import type React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../../src/locales/en.json';
import ValueEditor from '../../src/editors/ValueEditors';
import type { TestIo } from '../../src/generated/catala_types';
import {
  renderEditor,
  expectValueKind,
  moneyVal,
  rv,
} from './test-helpers.tsx';
import { renderAtomicValue } from '../../src/test-case-editor/testCaseUtils';

// Catala decimals and money are exact; the editors must not round them
// through a binary float.
describe('RatEditor keeps decimals exact', () => {
  beforeEach(() => vi.clearAllMocks());

  it('emits the typed decimal digit for digit', () => {
    const { onValueChange } = renderEditor({ kind: 'TRat' });
    const input = screen.getByRole('textbox') as HTMLInputElement;
    fireEvent.change(input, { target: { value: '1234567890.123456789' } });
    expectValueKind(onValueChange, 'Decimal', '1234567890.123456789');
  });

  it('emits 0.1 as 0.1', () => {
    const { onValueChange } = renderEditor({ kind: 'TRat' });
    const input = screen.getByRole('textbox') as HTMLInputElement;
    fireEvent.change(input, { target: { value: '0.1' } });
    expectValueKind(onValueChange, 'Decimal', '0.1');
  });

  it('shows a decimal without a finite expansion as a valid fraction', () => {
    renderEditor({ kind: 'TRat' }, vi.fn(), {
      value: rv({ kind: 'Decimal', value: '-1/3' }),
    });
    const input = screen.getByRole('textbox') as HTMLInputElement;
    expect(input.value).toBe('-1/3');
    fireEvent.blur(input);
    expect(input.className).not.toContain('invalid');
  });

  it('accepts a typed fraction, but not a zero denominator', () => {
    const { onValueChange } = renderEditor({ kind: 'TRat' });
    const input = screen.getByRole('textbox') as HTMLInputElement;
    fireEvent.change(input, { target: { value: '2/3' } });
    expectValueKind(onValueChange, 'Decimal', '2/3');
    fireEvent.change(input, { target: { value: '2/0' } });
    expectValueKind(onValueChange, 'Unset');
  });

  it('keeps the typed text when the value comes back in another spelling', () => {
    const onValueChange = vi.fn();
    const editor = (value?: TestIo['value']): React.ReactElement => (
      <IntlProvider locale="en" messages={enMessages}>
        <ValueEditor
          testIO={{ typ: { kind: 'TRat' }, value }}
          onValueChange={onValueChange}
          currentPath={[]}
          diffs={[]}
        />
      </IntlProvider>
    );
    const { rerender } = render(editor());
    const input = screen.getByRole('textbox') as HTMLInputElement;
    fireEvent.change(input, { target: { value: '13.' } });
    expectValueKind(onValueChange, 'Decimal', '13.');
    // The parser writes it back as 13.0
    rerender(editor({ value: rv({ kind: 'Decimal', value: '13.0' }) }));
    expect(input.value).toBe('13.');
    // ...while a different value replaces the text
    rerender(editor({ value: rv({ kind: 'Decimal', value: '1/3' }) }));
    expect(input.value).toBe('1/3');
  });
});

describe('MoneyEditor keeps cents exact', () => {
  beforeEach(() => vi.clearAllMocks());

  // A double resolves cents only below 2^46 units
  it('emits the cents of a large amount', () => {
    const { onValueChange } = renderEditor({ kind: 'TMoney' });
    const input = screen.getByRole('textbox') as HTMLInputElement;
    fireEvent.change(input, { target: { value: '70368744177664.01' } });
    expectValueKind(onValueChange, 'Money', 7036874417766401);
  });

  it('shows the cents of a large amount', () => {
    renderEditor({ kind: 'TMoney' }, vi.fn(), {
      value: moneyVal(7036874417766401),
    });
    const input = screen.getByRole('textbox') as HTMLInputElement;
    expect(input.value).toBe('70368744177664.01');
  });
});

describe('renderAtomicValue is exact', () => {
  it('renders money from integer cents', () => {
    expect(renderAtomicValue(moneyVal(7036874417766401))).toBe(
      '70368744177664.01'
    );
    expect(renderAtomicValue(moneyVal(-5))).toBe('-0.05');
  });

  it('renders decimals as spelled', () => {
    expect(
      renderAtomicValue(rv({ kind: 'Decimal', value: '1234567890.123456789' }))
    ).toBe('1234567890.123456789');
    expect(renderAtomicValue(rv({ kind: 'Decimal', value: '1/3' }))).toBe(
      '1/3'
    );
  });
});
