import {h, render, FunctionalComponent} from "preact"
import cls from "classnames"

type F<T> = FunctionalComponent<T>;

interface ItemProps {
  active: boolean,
  disabled: boolean,
  onClick: () => void,
  children: any,
}

export const Item: F<ItemProps> = (props) =>
  <a href=""
    class={cls(
      "btn-link list-group-item list-group-item-action",
      {active: props.active},
      {disabled: props.disabled},
    )}
    onClick={ev => {
      ev.preventDefault();
      !props.disabled && props.onClick();
    }}
  >
    {props.children}
  </a>;

export const Selector: F<{}> = (props) =>
  <div class="list-group">
    {props.children}
  </div>;
