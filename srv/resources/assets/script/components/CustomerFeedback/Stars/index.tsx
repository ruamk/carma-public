import {h, FunctionalComponent} from 'preact'
import cls from 'classnames'
import './index.less'

type Props = {
  stars: number
  value: number
  onChange: (value: number) => void
}

export const Stars: FunctionalComponent<Props> =
  ({stars, value, onChange}) =>
    <div class='stars'>
      { rangeMap(stars, i =>
        <div
          class={cls('star', {golden: i+1 <= value})}
          onClick={() => onChange(i+1)}/>)
      }
    </div>

// Map `fn` over an integer range [0..n).
function rangeMap<T>(n: number, fn: ((i:number) => T)): T[] {
  return new Array(n).fill(0).map((_, i) => fn(i))
}
