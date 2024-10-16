# 📁 server.py -----
import json
from urllib.parse import quote_plus, urlencode

import uvicorn
from authlib.integrations.starlette_client import OAuth
from fastapi import FastAPI, Request, Depends, HTTPException
from fastapi.security import OAuth2PasswordBearer
from starlette import status
from starlette.config import Config
from starlette.middleware.sessions import SessionMiddleware
from starlette.responses import HTMLResponse, RedirectResponse
from starlette.staticfiles import StaticFiles
from starlette.templating import Jinja2Templates

templates = Jinja2Templates(directory=".")

# 👆 We're continuing from the steps above. Append this to your server.py file.
app = FastAPI()

config = Config('.env')  # Make sure you have your Auth0 credentials in a .env file

SESSION_SECRET_KEY = config("SESSION_SECRET_KEY", default=None)
if SESSION_SECRET_KEY is None:
    raise ValueError("SESSION_SECRET_KEY must be set")
# Add session middleware

app.add_middleware(SessionMiddleware, secret_key="your-secret-key")

# Mount static files
app.mount("/static", StaticFiles(directory="public"), name="static")
# Configure Auth0
oauth = OAuth(config)

oauth.register(
    "auth0",
    client_id=config("AUTH0_CLIENT_ID"),
    client_secret=config("AUTH0_CLIENT_SECRET"),
    client_kwargs={
        "scope": "openid profile email",
    },
    server_metadata_url=f'https://{config("AUTH0_DOMAIN")}/.well-known/openid-configuration'

)
# This will be used to get the access token from the request
oauth2_scheme = OAuth2PasswordBearer(tokenUrl="token")


@app.get("/login")
async def login(request: Request):
    redirect_uri = request.url_for("callback")  # This assumes you have a callback endpoint
    return await oauth.auth0.authorize_redirect(request, redirect_uri, _external=True)


@app.get("/callback")
async def callback(request: Request):
    token = await oauth.auth0.authorize_access_token(request)

    # Set access_token in HttpOnly cookie
    response = RedirectResponse(url="/")
    response.set_cookie(
        "access_token",
        token['access_token'],
        httponly=True,
        secure=True,  # for HTTPS
        samesite='lax',
        max_age=token['expires_in']
    )

    # Only send necessary info to the client
    client_info = {
        "Auth0UserInfo": token['userinfo'],
        "expires_at": token['expires_at']
    }
    request.session['Auth0Info'] = client_info
    return response


@app.get("/logout")
async def logout(request: Request):
    request.session.clear()
    return RedirectResponse(
        "https://" + config("AUTH0_DOMAIN")
        + "/v2/logout?"
        + urlencode(
            {
                "returnTo": request.url_for("home"),
                "client_id": config("AUTH0_CLIENT_ID"),
            },
            quote_via=quote_plus,
        )
    )


@app.get("/", response_class=HTMLResponse)
async def home(request: Request):
    auth0_info = request.session.get('Auth0Info')
    return templates.TemplateResponse(
        "public/index.html",
        {
            "request": request,
            "user_json": json.dumps(auth0_info) if auth0_info else "null"
        }
    )


async def get_current_user(token: str = Depends(oauth2_scheme)):
    try:
        user = await oauth.auth0.parse_id_token(token)
        return user
    except Exception:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid authentication credentials",
            headers={"WWW-Authenticate": "Bearer"},
        )


@app.post("/save")
async def save_data(data: dict, current_user: dict = Depends(get_current_user)):
    # Here you can save the data, associated with the current user
    # For this example, we'll just return the data and user info
    return {"message": "Data saved", "data": data, "user": current_user}


if __name__ == "__main__":
    uvicorn.run(app, host="localhost", port=8000)
