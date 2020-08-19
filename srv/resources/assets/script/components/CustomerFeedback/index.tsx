import {h, render, FunctionalComponent} from "preact"
import {useState} from "preact/hooks"

import {Modal} from "./Modal"
import {Selector, Item} from "./Selector"


interface Props {
  onClose: () => void,
}

const CustomerFeedback: FunctionalComponent<Props> = (props) => {
  const [value, setValue] = useState(null);
  const [comment, setComment] = useState("");
  const [readonly, setReadonly] = useState(false);

  const onSave = async () => {
    setReadonly(true);
    try {
      await new Promise((resolve, reject) => {
        setTimeout(
          () => reject(new Error("Что-то пошло не так.")),
          1000);
      });
    } finally {
      setReadonly(false);
    }
  };

  const values = [ // FIXME: get those from dictionary
    {val: 1, label: "🙂 Клиент доволен"},
    {val: 2, label: "🙁 Клиент не доволен"},
    {val: 3, label: "🤐 Клиент не отвечает"}
  ];

  return (
    <Modal
      title="Оценка качества обслуживания"
      onClose={props.onClose}
      onSave={onSave}
      canSave={value && !readonly}
    >
      <form>
        <Selector>
          { values.map(({val, label}) =>
            <Item
              active={value == val}
              disabled={readonly}
              onClick={() => setValue(val)}>
              <h3>{label}</h3>
            </Item>
          )}
        </Selector>
        <div>
          <label>Комментарий</label>
          <textarea
            class="form-control"
            rows={6}
            readOnly={readonly}
            value={comment}
            onInput={ev => setComment((ev.target as HTMLInputElement).value)}
          />
        </div>
      </form>
    </Modal>);
}


export function show(
  caseId: number,
  serviceId: number | null,
  readonly: boolean
) {
  const container = document.getElementById("injected-modal");
  let modal = null;
  const destroy = () => render(null, container, modal);

  const dialog = <CustomerFeedback onClose={destroy}/>;
  modal = render(dialog, container);
}
