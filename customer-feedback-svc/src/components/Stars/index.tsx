import {h} from 'preact'
import cls from 'classnames'
import {F} from '../../types'
import './index.scss'

type Props = {
  stars: number
  value: number
  onChange: (value: number) => void
}

export const Stars: F<Props> = ({stars, value, onChange}) =>
  <div class='stars'>
    { rangeMap(stars, i =>
      <button
        class={cls('star', {golden: i+1 <= value})}
        onClick={() => onChange(i+1)}/>)
    }
  </div>

// Map `fn` over an integer range [0..n).
function rangeMap<T>(n: number, fn: ((i:number) => T)): T[] {
  return new Array(n).fill(0).map((_, i) => fn(i))
}
