import {h, render, FunctionalComponent } from "preact"
import {useState} from "preact/hooks"
import moment from "moment"

import {Modal, ModalWrapper} from "./Modal"
import {Spinner} from "./Spinner"
import {Selector, Item} from "./Selector"


interface Feedback {
  value: number,
  comment: string,
  ctime: null | string,
  realName: null | string,
  login: null | string,
}

interface Props {
  data: null | Feedback,
  onSave: (Feedback) => Promise<any>,
  onClose: () => void,
}


const CustomerFeedback: FunctionalComponent<Props> = (props) => {
  const {data} = props;
  const [value, setValue] = useState(data ? data.value : null);
  const [comment, setComment] = useState(data ? data.comment : "");
  const [readonly, setReadonly] = useState(!!data);

  const onSave = async () => {
    setReadonly(true);
    try {
      await props.onSave({value, comment});
      props.onClose();
    } finally {
      setReadonly(false);
    }
  };

  const values = [ // FIXME: get those from dictionary
    {val: 1, label: "🙂 Клиент доволен"},
    {val: 2, label: "🙁 Клиент не доволен"},
    {val: 3, label: "🤐 Клиент не отвечает"}
  ];

  const field = (lbl, val) =>
    <div>
      <label>{lbl}:&nbsp;</label>
      <span>{val}</span>
    </div>;

  return (
    <Modal
      title="Оценка качества обслуживания"
      onClose={props.onClose}
      onSave={onSave}
      canSave={value && !readonly}
    >
      <form>
        { data && [
          field("Оператор", `${data.realName} (${data.login})`),
          field(
            "Отзыв оставлен",
            moment(data.ctime).format("YYYY-MM-DD HH:mm"))
        ]}

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


const GENERIC_ERROR = new Error("Что-то пошло не так");

async function loadFeedback(caseId, serviceId) {
  const r = await fetch(
    "/customerFeedback?" + new URLSearchParams({caseId, serviceId})
  );
  if(!r.ok) throw GENERIC_ERROR;
  else return r.json();
}


function saveFeedback(caseId, serviceId) {
  return async ({value, comment}) => {
    const r = await fetch("/customerFeedback", {
      method: "POST",
      headers: {
        "Accept": "application/json",
        "Content-Type": "application/json"
      },
      body: JSON.stringify({caseId, serviceId, value, comment})
    });

    if(!r.ok) throw GENERIC_ERROR;
    else return r.json();
  };
}


export async function show(
  caseId: number,
  serviceId: number | null,
  readonly: boolean
) {
  const container = document.getElementById("injected-modal");
  const destroy = () => render(null, container);

  // FIXME: cancel loadFeedback in onClose
  const spinner = <ModalWrapper onClose={destroy}>
    <Spinner /> Загрузка...
  </ModalWrapper>;

  render(spinner, container);

  const fb = await loadFeedback(caseId, serviceId);
  // FIXME: catch err and show it

  const dialog = <CustomerFeedback
    data={fb.length > 0 ? fb[0] : null}
    onClose={destroy}
    onSave={saveFeedback(caseId, serviceId)}/>;

  render(dialog, container);
}
