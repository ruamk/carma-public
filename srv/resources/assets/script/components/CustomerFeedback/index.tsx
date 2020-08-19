import {h, render, FunctionalComponent} from "preact"


interface Props {
  onClose: () => void,
}

const CustomerFeedback: FunctionalComponent<Props> = (props) => {
  return (
    <div>
      <div class="modal-backdrop fade active in" onClick={props.onClose}/>
    </div>);
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
