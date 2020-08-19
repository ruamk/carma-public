import {h, render, FunctionalComponent} from "preact"
import {useState} from "preact/hooks"
import cls from "classnames"

type F<T> = FunctionalComponent<T>;

interface Props {
  title: string,
  onClose: () => void,
  onSave:  () => Promise<any>,
  canSave: boolean,
}


const Header: F<Props> = ({title, onClose}) =>
  <div class="modal-header">
    <button type="button" class="close" onClick={onClose}>
      <span aria-hidden="true">×</span>
      <span class="sr-only">Закрыть</span>
    </button>
    <h4 class="modal-title">{title}</h4>
  </div>;


const Spinner: F<{}> = (_) =>
  <span class="mr-2 glyphicon glyphicon-refresh glyphicon-refresh-animate"/>;


const Footer: F<Props> = ({onClose, onSave, canSave}) => {
  const [inProgress, setInProgress] = useState(false);
  const [err, setErr] = useState(null);

  const doSave = async () =>  {
    setInProgress(true);
    try { await onSave(); }
    catch(e) { setErr(e); }
    finally { setInProgress(false); }
  };

  return (
    <div class="modal-footer">
      { inProgress && <Spinner/> }
      <button type="button"
        class={cls("btn btn-primary", {disabled: !canSave})}
        onClick={canSave && doSave}>
        Сохранить
      </button>
      <button type="button" class="btn btn-secondary" onClick={onClose}>
        Отменить
      </button>
      { err && <div class="mt-2 alert alert-danger">{err.message}</div> }
    </div>);
}


// TODO: handle Escape btn
export const Modal: F<Props> = (props) =>
  <div>
    <div class="modal-backdrop fade active in" onClick={props.onClose}/>
    <div class="modal show active in" role="dialog">
      <div class="modal-dialog" role="document">
        <div class="modal-content">
          <Header {...props} />
          <div class="modal-body">
            {props.children}
          </div>
          <Footer {...props} />
        </div>
      </div>
    </div>
  </div>;
