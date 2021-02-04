import {h, render, FunctionalComponent } from "preact"
import {useState} from "preact/hooks"
import moment from "moment"

import {Stars} from "./Stars"
import {Modal} from "./Modal"


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
  readonly: boolean,
  data: null | Feedback,
  onSave: (Feedback) => Promise<any>,
  onClose: () => void,
}


export const ServiceFeedback: FunctionalComponent<Props> = (props) => {
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

        <p>Оцените работу оператора</p>
        <Stars stars={5} value={operValue} onChange={setOperValue}/>

        <p>Оцените работу механика</p>
        <Stars stars={5} value={techValue} onChange={setTechValue}/>

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
