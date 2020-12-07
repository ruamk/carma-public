import {h, render, FunctionalComponent } from "preact"
import {useState} from "preact/hooks"
import moment from "moment"

import {Modal, ModalWrapper} from "./Modal"
import {Spinner} from "./Spinner"

import "./index.less"


interface Feedback {
  response: {
    operValue: number | null,
    techValue: number | null,
    comment: string | null,
  },
  ctime: string | null,
  realName: string | null,
  login: string | null,
}

interface Props {
  data: null | Feedback,
  onSave: (Feedback) => Promise<any>,
  onClose: () => void,
}


interface ValueProps {
  question: string,
  group: string,
  value: number,
  onChange: (val: number) => void,
}

const Value: FunctionalComponent<ValueProps> = props =>
  <div>
    <p>{props.question}</p>
    <div class="satisfaction-value">
      {[1, 2, 3, 4, 5].map(val =>
        <label>
          <input
            type="radio"
            name={props.group}
            value={val}
            checked={props.value === val}
            onChange={_ => props.onChange(val)}
          />
          {val}
        </label>
      )}
    </div>
  </div>


const CustomerFeedback: FunctionalComponent<Props> = (props) => {
  const {data} = props;
  const [operValue, setOperValue] = useState(data?.response.operValue);
  const [techValue, setTechValue] = useState(data?.response.techValue);
  const [comment, setComment] = useState(data?.response.comment || "");
  const [readonly, setReadonly] = useState(!!data);

  const onSave = async () => {
    setReadonly(true);
    try {
      await props.onSave({operValue, techValue, comment});
      props.onClose();
    } finally {
      setReadonly(false);
    }
  };

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
      canSave={operValue && techValue && !readonly}
    >
      <form>
        { data && [
          field("Оператор", `${data.realName} (${data.login})`),
          field(
            "Отзыв оставлен",
            moment(data.ctime).format("YYYY-MM-DD HH:mm"))
        ]}

        <Value
          question="Оцените работу оператора"
          group="operator"
          value={operValue}
          onChange={setOperValue}
        />
        <Value
          question="Оцените работу механика"
          group="tech"
          value={techValue}
          onChange={setTechValue}
        />
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
  return async (response) => {
    const r = await fetch("/customerFeedback", {
      method: "POST",
      headers: {
        "Accept": "application/json",
        "Content-Type": "application/json"
      },
      body: JSON.stringify({caseId, serviceId, response})
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
