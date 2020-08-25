import {h, FunctionalComponent} from "preact"
import {useState} from "preact/hooks"
import cls from "classnames"

import {Spinner} from "./Spinner"

type F<T> = FunctionalComponent<T>;

interface Closable {
  onClose: () => void
}

interface Props extends Closable {
  title: string,
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


export const ModalWrapper: F<Closable> = (props) =>
  // NB. `stopPropagation` is to prevent modal from closing when clicked
  // somewere on the dialog itself.
  <div>
    <div class="modal-backdrop fade active in"/>
    <div class="modal show" role="dialog" onClick={props.onClose}>
      <div class="modal-dialog" role="document" onClick={e => e.stopPropagation()}>
        <div class="modal-content">
          {props.children}
        </div>
      </div>
    </div>
  </div>;


export const Modal: F<Props> = (props) =>
  <ModalWrapper onClose={props.onClose}>
    <Header {...props} />
    <div class="modal-body">
      {props.children}
    </div>
    <Footer {...props} />
  </ModalWrapper>;
