import {FunctionalComponent, h} from 'preact'
import cn from 'classnames'

export type Props = {
  buttons: BtnSpec[]
  activeButtons: Set<string>
  onChange(key: string): void
}

export type BtnSpec = {
  key: string
  name: string
  icon: string
}

// Shows a bunch of toggle buttons
export const BtnGroup: FunctionalComponent<Props> = (props) => {
  const btn = (b) =>
    <div
      class={cn('btn btn-default', {active: props.activeButtons.has(b.key)})}
      title={b.name}
      onClick={() => props.onChange(b.key)}
    >
      <i class={`glyphicon glyphicon-${b.icon}`}/>
    </div>

  return (
    <div class='btn-group btn-group-sm' role='group'>
      {props.buttons.map(btn)}
    </div>)
}
