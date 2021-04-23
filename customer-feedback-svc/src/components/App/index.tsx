import {h, Fragment} from 'preact'
import {useEffect, useState} from 'preact/hooks'
import cls from 'classnames'
import {Stars} from '../Stars'
import * as Type from '../../types'
import './index.scss'

enum Page { Welcome, Stars, Done }

export const App = () => {
  const [currentPage, setCurrentPage] = useState(Page.Welcome)
  const [result, setResult] = useState(null)
  const [errorMessage, setErrorMessage] = useState('')
  const [isSaving, setIsSaving] = useState(false)

  const key = window.location.hash.replace('#', '')
  const apiUrl = `/api/${key}`

  const loadFormData = async () => {
    try {
      const res = await fetch(apiUrl, {method: 'GET'})
      if(res.ok) {
        const jsn = await res.json()
        setResult(jsn)
        setCurrentPage(Page.Stars)
      } else {
        setErrorMessage('Неправильная ссылка или время действия истекло.')
      }
    } catch {
      setErrorMessage('Ошибка загрузки данных, повторите попытку позже.')
    }
  }

  useEffect(() => {loadFormData()}, [])

  const saveResult = async () => {
    try {
      setIsSaving(true)
      const r = await fetch(apiUrl, {
        method: 'POST',
        body: JSON.stringify(result)
      })

      if(r.ok) {
        setCurrentPage(Page.Done)
      } else {
        throw new Error()
      }
    } catch {
      setErrorMessage('Отправка невозможна, проверьте ваше соединение или повторите попытку позже!')
    }
    setIsSaving(false)
  }

  const canSaveResult = result && result.operValue && result.techValue

  return (
    <section>
      <div class='column is-narrow'>
        {currentPage === Page.Welcome &&
          <Welcome isLoading={!result} onCurrentPage={setCurrentPage}/>
        }

        {currentPage === Page.Stars &&
          <Fragment>
            <Question
              text="Оцените работу оператора колл-центра"
              value={result.operValue}
              onChange={v => setResult(Object.assign({}, result, {operValue: v}))}
            />
            <Question
              text="Оцените работу техника"
              value={result.techValue}
              onChange={v => setResult(Object.assign({}, result, {techValue: v}))}
            />
            <div class='container has-text-centered mt-6'>
              <textarea class='textarea'
                onInput={e =>
                  setResult(Object.assign({}, result,
                    {comment: (e.target as HTMLInputElement).value}
                ))}
                value={result.comment || ''}
                placeholder='Комментарий'/>
            </div>
            <div class='container has-text-centered mt-6'>
              <button
                class={cls('button is-primary', {'is-loading': isSaving})}
                disabled={!canSaveResult || isSaving}
                title={canSaveResult ? '' : 'Пожалуйста выберите ответ!'}
                onClick={saveResult}
              >
                {isSaving ? 'Отправляется' : 'Отправить'}
              </button>
            </div>
          </Fragment>
        }

        {currentPage === Page.Done && <Done/> }
      </div>

      {errorMessage &&
        <div class='notification is-danger has-text-centered'>
          <button class='delete' onClick={() => setErrorMessage('')}/>
          {errorMessage}
        </div>
      }
    </section>
  )
}


const Welcome: Type.F<{isLoading: boolean, onCurrentPage: (i: any) => void }> =
  ({isLoading, onCurrentPage}) =>
  <div class='container has-text-centered mt-6'>
    <h1 class='title'>Здравствуйте, оцените оказанную вам услугу</h1>
    <h2 class='subtitle'>Пройдите небольшой опрос</h2>
    <button
      class={cls('button is-primary', {'is-loading': isLoading})}
      onClick={() => onCurrentPage(Page.Stars)} disabled={isLoading}
    >
      Пройти опрос
    </button>
  </div>


const Done = () =>
  <div class='container has-text-centered mt-6'>
    <div class='done-img'/>
    <h1 class='title'>Спасибо за ваши ответы!</h1>
  </div>


type QuestionProps = {
  text: string
  value: number
  onChange: (e: number) => void
}

const Question: Type.F<QuestionProps> = ({text, value, onChange}) =>
  <div class='container has-text-centered mt-6'>
    <h2 class='subtitle'>{text}</h2>
    <Stars stars={5}
           value={value}
           onChange={onChange}/>
  </div>
