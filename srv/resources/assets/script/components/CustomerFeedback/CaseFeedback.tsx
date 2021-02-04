import {h, render, FunctionalComponent } from "preact"
import {useState} from "preact/hooks"
import moment from "moment"

import {Modal} from "./Modal"


interface Feedback {
  response: {
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


export const CaseFeedback: FunctionalComponent<Props> = (props) => {
  const {data} = props;
  const [comment, setComment] = useState(data?.response.comment || "");
  const [readonly, setReadonly] = useState(!!data);

  const onSave = async () => {
    setReadonly(true);
    try {
      await props.onSave({comment});
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
      canSave={comment && !readonly}
    >
      <form>
        { data && [
          field("Оператор", `${data.realName} (${data.login})`),
          field(
            "Отзыв оставлен",
            moment(data.ctime).format("YYYY-MM-DD HH:mm"))
        ]}

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
